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

%%% This is a minimal MIME parser, witten to support what is needed
%%% to support SOAP with attachments as specified in W3C Note "SOAP 
%%% Messages with Attachments".
-module(soap_mime).

-export([decode/2]).
-export([encode/2]).
-export([parse_mime_parameters/1]).
-export([mime_body/3]).
-export([mime_content_type/0]).
-export([test/2]).

-type parameter_name() :: list().
-type parameter_value() :: list().
-type soap_attachment() :: soap:soap_attachment().

%% Assumption: no need to generate something random each 
%% time, good enough to do it once.
-define(MIME_BOUNDARY, "iIQLXgstbBC1LLaDjlFpbkpSfNejmNQGCHTE7dl1").

-spec decode(Message::binary(), Boundary::binary()) ->
    [soap_attachment()].
decode(Message, Boundary) ->
    Boundary2 = [<<"\r\n--", Boundary/binary, "\r\n">>,
                 <<"\r\n--", Boundary/binary, "--">>,
                 <<"--", Boundary/binary, "\r\n">>],
    case binary:split(Message, Boundary2, [global]) of
        [_ | Parts] when length(Parts) > 1 ->
            lists:flatten([decode_part(P) || P <- lists:droplast(Parts), 
                           P /= <<>>]);
        _ ->
            []
    end.

-spec encode(Parts::[soap_attachment()], Boundary::binary()) ->
    binary().
encode(Parts, Boundary) ->
    Boundary2 = <<"\r\n--", Boundary/binary>>,
    encode_parts(lists:reverse(Parts), Boundary2, <<Boundary2/binary, "--">>).


%% from rfc1341:
%% Content-Type := type "/" subtype *[";" parameter] 
%% 
%% %% In this case Content-Type is always multipart/related 
%% %% and it has already been taken off. This only deals with the
%% %% parameters.
%%
%% parameter := attribute "=" value 
%% attribute := token 
%% value := token / quoted-string 
%% token := 1*<any CHAR except SPACE, CTLs, or tspecials> 
%%
%% Parameter names are converted to lower case.
%%
%% Note that this implementation does not consider all possible 
%% scenario's
-spec parse_mime_parameters(string()) -> [{parameter_name(), parameter_value()}].
parse_mime_parameters(String) ->
    parameters(String, []).


-spec mime_content_type() -> string().
mime_content_type() ->
    "multipart/related; type=\"text/xml\"; boundary=\""
    ?MIME_BOUNDARY 
    "\"". 

-spec mime_body(binary(), [soap:http_header()], [soap_attachment()]) -> binary().
mime_body(Message_body, HTTP_Headers, Attachments) ->
    encode([{HTTP_Headers, Message_body} | Attachments], 
           list_to_binary(?MIME_BOUNDARY)).

%% internal functions

%% When an unexpected character (or []) occurs,
%% parsing stops and results to that poitn are returned.
parameters([], Acc) ->
    Acc;
parameters([Whitespace | T], Acc) when Whitespace =:= 32;
                                       Whitespace =:= 8 ->
    parameters(T, Acc);
parameters([$; | T], Acc) ->
    parameters(T, Acc);
parameters([Illegal | _T], Acc) when Illegal =:= $=;
                                     Illegal =:= $" ->
    Acc;
parameters(T, Acc) ->
    token(T, [], Acc).

token([], _, Acc) ->
    Acc;
token([Whitespace | T], Token, Acc) when Whitespace =:= 32;
                                         Whitespace =:= 8 ->
    separator(T, Token, Acc);
token([Separator | T], Token, Acc) when Separator =:= $= ->
    separated(T, string:to_lower(lists:reverse(Token)), Acc);
token([Char | T], Token, Acc) ->
    token(T, [Char | Token], Acc).

separator([Whitespace | T], Token, Acc) when Whitespace =:= 32;
                                             Whitespace =:= 8 ->
    separator(T, Token, Acc);
separator([Separator | T], Token, Acc) when Separator =:= $= ->
    separated(T, string:to_lower(lists:reverse(Token)), Acc);
separator(_, _, Acc) ->
    Acc.

separated([], _, Acc) ->
 Acc;
separated([Whitespace | T], Token, Acc) when Whitespace =:= 32;
                                             Whitespace =:= 8 ->
    separated(T, Token, Acc);
separated([Illegal | _T], _, Acc) when Illegal =:= $=;
                                       Illegal =:= $; ->
    Acc;
separated([$" | T], Token, Acc) ->
    quoted_value(T, [], Token, Acc);
separated(Tail, Token, Acc) ->
    value(Tail, [], Token, Acc).

value([], Value, Token, Acc) ->
    [{Token, lists:reverse(Value)} | Acc];
value([Illegal | _], _, _, Acc) when Illegal =:= $=;
                                     Illegal =:= 32;
                                     Illegal =:= 8;
                                     Illegal =:= $" ->
    Acc;
value([$; | T], Value, Token, Acc) ->
    parameters(T, [{Token, lists:reverse(Value)} | Acc]);
value([Char | T], Value, Token, Acc) ->
    value(T, [Char | Value], Token, Acc).

quoted_value([], _Value, _Token, Acc) ->
    Acc;
quoted_value([$\\,Char | T], Value, Token, Acc) ->
    quoted_value(T, [Char | Value], Token, Acc);
quoted_value([$" | T], Value, Token, Acc) ->
    parameters(T, [{Token, lists:reverse(Value)} | Acc]);
quoted_value([Char | T], Value, Token, Acc) ->
    quoted_value(T, [Char | Value], Token, Acc).

decode_part(P) ->
    case binary:split(P, <<"\r\n\r\n">>) of
        [Headers, Body] ->
            {decode_headers(Headers), Body};
        [<<"\r\n", Body/binary>>] ->
            {[], Body}
    end.

decode_headers(Header_part) ->
    Headers = binary:split(Header_part, <<"\r\n">>, [global, trim_all]),
    [decode_header(H) || H <- Headers].

decode_header(Header) ->
    [Name, Value] = binary:split(Header, <<":">>),
    {decode_name(Name), decode_value(Value)}.

decode_name(Name) ->
    binary_to_list(Name).

decode_value(Value) ->
    string:strip(binary_to_list(Value), left).

encode_parts([], _Boundary, Acc) ->
    Acc;
encode_parts([Part | T], Boundary, Acc) ->
    Encoded = encode_part(Part),
    encode_parts(T, Boundary, << Boundary/binary, Encoded/binary, Acc/binary>>).

encode_part({Headers, Body}) ->
    Encoded_headers = encode_headers(Headers),
    Body_binary = iolist_to_binary(Body),
    <<Encoded_headers/binary, "\r\n\r\n", Body_binary/binary>>.

encode_headers(Headers) ->
    encode_headers(Headers, <<>>).

encode_headers([], Acc) ->
    Acc;
encode_headers([{Name, Value} | T], Acc) ->
    Name_b = iolist_to_binary(Name),
    Value_b = iolist_to_binary(Value),
    encode_headers(T, <<Acc/binary, "\r\n", 
                   Name_b/binary, ": ", Value_b/binary>>).

-spec test(File::string(), Boundary::string()) -> binary().
test(File, Boundary) ->
    Boundary2 = list_to_binary(Boundary),
    {ok, Binary} = file:read_file(File),
    Decoded = decode(Binary, Boundary2),
    [print_part(P) || P <- Decoded],
    encode(Decoded, Boundary2).

print_part({Headers, Body}) ->
    io:format("~p~n", [Headers]),
    io:format("++~p++\n", [Body]). 
