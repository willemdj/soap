%%
%% %CopyrightBegin%
%%
%% Copyright Hillside Technology Ltd. 2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%% Provides a couple of options for the parsing of the SOAP header and body.
%%%
%%% Each of the functions returns a tuple: {Sax_callback, Initial_state}
%%% 
-module(soap_parsers).

-export([simple_form/0, 
         very_simple_form/0, 
         skip/1, 
         data_mapper/1, 
         map/0]).

-export([test/0, 
         tests/0]).

-type sax_callback_fun() :: fun((erlsom:sax_event(), any()) -> any()).

-record(mState, {stack, nameFun, options}).
-record(sState, {depth = 0, value}).
-record(attribute, {localName, prefix = [], uri = [], value}).

%%% ============================================================================
%%% Exported functions
%%% ============================================================================

%% translates the XML to 'simple_form' 
-spec simple_form() -> {sax_callback_fun(), undefined}.
simple_form() ->
    {fun erlsom_simple_form:callback/2, undefined}.

%% like simple_form, but without the {Namespace} prefix.
-spec very_simple_form() -> {sax_callback_fun(), any()}.
very_simple_form() ->
    Namefun = 
        fun(Name, _, _) -> 
            Name
        end,
    {fun erlsom_simple_form:callback/2, [{name_function, Namefun}]}.

%% performs data mapping in accordance with the Model
-spec data_mapper(erlsom:model()) -> {sax_callback_fun(), any()}.
data_mapper(Model) ->
    Start_state = erlsom_parse:new_state(Model),
    {fun erlsom_parse:xml2StructCallback/2, Start_state}.

%% This will simply ignore the XML, and produce Result.
-spec skip(any()) -> {sax_callback_fun(), any()}.
skip(Result) ->
    {fun skip_callback/2, Result}.

%% translates the XML to a map
%%
%% Follows http://www.xml.com/lpt/a/1658
%%
%% XML                              Map
%% <e/>                             #{"e" => undefined}
%% <e>text</e>                      #{"e" => <<"text">>}
%% <e name="value"/>                #{"e" => #{"@name" => <<"value">>}}
%% <e name="value">text</e>         #{"e" => #{"@name" => <<"value">>, "#text => <<"text">>}}
%% <e><a>text</a><b>text</b></e>    #{"e" => #{"a" => <<"text", <<"b" => <<"text">>}}
%% <e><a>text</a><a>text</a></e>    #{"e" => #{"a" => [<<"text", <<"text">>] }}
%% <e>text<a>text</a></e>           #{"e" => #{"#text" => <<"text">>, "a" => <<"text">>}}
-spec map() -> {sax_callback_fun(), any()}.
map() ->
    {fun callback/2, undefined}.


%%% ============================================================================
%%% Internal functions
%%%
%%% The rest of this module deals with the 'map' and the 'skip' parsers. 
%%% ============================================================================

%% map
callback(Event, State) ->
    try
        case Event of
            startDocument -> 
                case State of
                    #mState{} ->
                        State;
                    [{name_function, NameFun}] ->
                        new_state(NameFun);
                    _ ->
                        new_state(fun nameFun/4)
                end;
            {startElement, _Uri, _LName, _Prfx, _Atts} ->
                startElement(Event, State);
            {endElement, _Uri, _LocalName, _Prefix} ->
                endElement(Event, State);
            {characters, _Characters} ->
                characters(Event, State);
            {ignorableWhitespace, _Characters} -> State;
            {processingInstruction, _Target, _Data} -> State;
            {startPrefixMapping, _Prefix, _URI} -> 
                State;
            {endPrefixMapping, _Prefix} ->
                State;
            endDocument -> 
                case State of 
                    {result, Root} ->
                        Root;
                    _Else ->
                        throw({error, "unexpected end"})
                end;
            {error, Message} ->
                throw(Message);
            {'EXIT', Message} ->
                exit(Message)
        end
    catch
        error:Reason -> 
            throwError(error, {Reason,erlang:get_stacktrace()}, Event, State);
        Class:Exception -> throwError(Class, Exception, Event, State)
    end.

%% skip
skip_callback(Event, State) ->
    try
        case Event of
            startDocument -> 
                case State of
                    #sState{} ->
                        State;
                    Value ->
                        #sState{value = Value}
                end;
            {startElement, _Uri, _LocalName, _Prefix, _Attributes} ->
                Depth = State#sState.depth,
                State#sState{depth = Depth + 1};
            {endElement, _Uri, _LocalName, _Prefix} ->
                Depth = State#sState.depth,
                case Depth of 
                    1 ->
                        {result, State#sState.value};
                    _ ->
                        State#sState{depth = Depth - 1}
                end;
            endDocument -> 
                case State of 
                    {result, Value} ->
                        Value;
                    _Else ->
                        throw({error, "unexpected end"})
                end;
            {error, Message} ->
                throw(Message);
            {'EXIT', Message} ->
                exit(Message);
            _ ->
                State
        end
    catch
        error:Reason -> 
            throwError(error, {Reason,erlang:get_stacktrace()}, Event, State);
        Class:Exception -> throwError(Class, Exception, Event, State)
    end.

startElement({startElement, Uri, LocalName, Prefix, Attributes}, 
             State = #mState{stack = Stack, nameFun = NameFun}) ->
    Name = NameFun(LocalName, Uri, Prefix, element),
    State#mState{stack = [{Name, 
                           processAttributes(Attributes, State)} | Stack]}.

%% Don't wait for the 'endDocument', when we are ready we are ready.
%% This way the soap_server_handler can parse more than one 
%% consecutive header (otherwise it would have not way to know that
%% when the end of the first header is reached).
endElement({endElement, _Uri, _LocalName, _Prefix},
           #mState{stack = [{Name, Elements}]}) ->
    Map = make_map(Elements),
    Document = make_map([{Name, Map}]),
    %% {result, Document} is a special value that signals to the 
    %% calling function that the parsing is done.
    {result, Document};

endElement({endElement, _Uri, _LocalName, _Prefix},
           State) ->
    #mState{stack = [{Name, Elements} | [{ParentName, ParentElements} | Tail]]} = State,
    Map = make_map(Elements),
    State#mState{stack = [{ParentName, 
                          [{Name, Map} | ParentElements]} | Tail]}.

characters({characters, Characters}, State) when is_list(Characters) ->
    characters({characters, unicode:characters_to_binary(Characters)}, State);
characters({characters, Characters},
           State = #mState{stack = [{Name, 
                                     [{characters, FirstBit} | OtherElements]
                                    } | Tail]})
        when is_binary(FirstBit) ->
    State#mState{stack = [{Name, 
                          [{characters, 
                            <<FirstBit/binary, 
                              Characters/binary>>} | OtherElements]
                          } | Tail]};
characters({characters, _Characters} = Event,
           State = #mState{stack = [{Name, Elements} | Tail]}) ->
    State#mState{stack = [{Name, [Event | Elements]} | Tail]}.

processAttributes(Attributes, State) ->
    processAttributes(Attributes, State, []).
processAttributes([], _State, Acc) ->
    Acc;
processAttributes([#attribute{localName=LocalName, uri=Uri, 
                              prefix = Prefix, value=Value} | Tail], 
                  State = #mState{nameFun = NameFun}, Acc) ->
    processAttributes(Tail, 
                      State, 
                      [{NameFun(LocalName, Uri, Prefix, attribute), 
                        binary_value(Value)} | Acc]).

binary_value(V) when is_binary(V) ->
    V;
binary_value(V) when is_list(V) ->
    unicode:characters_to_binary(V).


nameFun(Name, _Namespace, _Prefix, element) ->
    Name;
nameFun(Name, _Namespace, _Prefix, attribute) ->
    [$@ | Name].
    
make_map([]) ->
    undefined;
make_map([{characters, Element}]) ->
    Element;
make_map(Elements) ->
    Map = maps:new(),
    lists:foldl(fun insert_element/2, Map, Elements).

insert_element({characters, Mixed}, Map) ->
    put_in_map("#text", Mixed, Map);
insert_element({Name, Value}, Map) ->
    put_in_map(Name, Value, Map).

put_in_map(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, Existing_val} when is_list(Existing_val) ->
            maps:put(Key, [Value | Existing_val], Map);
        {ok, Existing_val} ->
            maps:put(Key, [Value, Existing_val], Map);
        error ->
            maps:put(Key, Value, Map)
    end.

throwError(Class, Exception, Event, 
           #mState{stack = Stack}) ->
%% "Error while parsing type " 
%% Take the ElementRecord at current state, and print the first element
    Message = [{exception, Exception},
               %% for each of the elements in ResultSoFar, 
               %% take the 'elementRecord' element and print the first element (the type).
               {stack, printStackTrace(Stack)},
               %% "Received: "
               {received, Event}],
    case Class of 
        'error' -> exit({error, Message});
        'throw' -> throw({error, Message});
        'exit' -> exit({error, Message})
    end;

throwError(Class, Exception, _Event, 
           _Something) ->
    case Class of 
        'error' -> exit({error, Exception});
        'throw' -> throw({error, Exception});
        'exit' -> exit({error, Exception})
    end.

printStackTrace(Stack) ->
    printStackTrace(Stack, []).
printStackTrace([], Acc) ->
    Acc;
printStackTrace([{Name, _, _} | Tail], Acc) ->
    printStackTrace(Tail, [{element, Name} | Acc]).

new_state(Namefun) ->
    #mState{stack = [], nameFun = Namefun, options = []}.

%%% A few simple tests

test() ->
    Xml = 
        <<"<L1>"
        "   <L2_1 attribute=\"attr_value\" attr2=\"v2\">text L2_1 nr 1</L2_1>"
        "   <L2_1>text L2_1 nr 2</L2_1>"
        "   <empty/>"
        "   <L2_2>text L2_2</L2_2>Mixed"
        "   <L2_3>"
        "        <L3>text L3</L3>"
        "   </L2_3>"
        "</L1>">>,
    {ok, Map, _} = erlsom:parse_sax(Xml, undefined, 
                                    fun callback/2, [{output_encoding, utf8}]),
    Map.

tests() ->
    Cases = [
        {<<"<e/>">>, #{"e" => undefined}},
        {<<"<e>text</e>">>, #{"e" => <<"text">>}},
        {<<"<e name=\"value\"/>">>, #{"e" => #{"@name" => <<"value">>}}},
        {<<"<e name=\"value\">text</e>">>, #{"e" => #{"@name" => <<"value">>, "#text" => <<"text">>}}},
        {<<"<e><a>text</a><b>text</b></e>">>, #{"e" => #{"a" => <<"text">>, "b" => <<"text">>}}},
        {<<"<e><a>text</a><a>text</a></e>">>, #{"e" => #{"a" => [<<"text">>, <<"text">>] }}},
        {<<"<e>text<a>text</a></e>">>, #{"e" => #{"#text" => <<"text">>, "a" => <<"text">>}}}],
    [test_case(Case) || Case <- Cases].

test_case({Xml, Result}) ->
    {Skip_fun, Skip_start} = skip(fixed),
    {ok, fixed, _} = erlsom:parse_sax(Xml, Skip_start, Skip_fun, []),
    {ok, Result, _} = 
        erlsom:parse_sax(Xml, undefined, 
                         fun callback/2, [{output_encoding, utf8}]),
    {Map_fun, Start_state} = map(),
    {ok, Result, _} = erlsom:parse_sax(Xml, Start_state, Map_fun, []).
