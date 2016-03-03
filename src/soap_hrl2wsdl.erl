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

%%% Translates an Erlang type specification to a WSDL.
%%%

-module(soap_hrl2wsdl).

-include("soap_wsdl_1_1.hrl").

-export([file/3, file/4]).

-type option() :: {target_namespace, string()}.

-record(operation, {
    name :: string(),
    in :: string(),
    out :: string()
}).


%%% ============================================================================
%%% Exported functions
%%% ============================================================================

%% Generate a WSDL from a (.hrl) file with Erlang type specifications
-spec file(Hrl_file::string(), Service_name::string(), Url::string()) -> ok.
file(File, Service_name, Url) ->
    file(File, Service_name, Url, []).

-spec file(Hrl_file::string(), Service_name::string(), Url::string(), 
           [option()]) -> ok.
file(File, Service_name, Url, Options) ->
    {ok, Binary} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary)),
    Base_name = filename:rootname(filename:basename(File), ".hrl"),
    Default_wsdl_name = Base_name ++ ".wsdl",
    Wsdl_name = proplists:get_value(wsdl_name, Options, Default_wsdl_name),
    Target_namespace = proplists:get_value('target_namespace', Options, 
                                           Url ++ "/" ++ Service_name), 
    Forms = split_forms(Tokens),
    ParsedForms = [erl_parse:parse_form(Form) || Form <- Forms],
    Ok_forms = [Form || {ok, Form} <- ParsedForms],
    Records = lists:filter(fun(F) -> element(3, F) == record end, Ok_forms),
    %% Generate the xml schema part:
    Types = erlsom_type2xsd:translate_forms(Records, 
                                            [{target_namespace, 
                                             {Target_namespace, "tns"}}]),
    Specs = lists:filter(fun(F) -> element(3, F) == spec end, Ok_forms),
    Functions = make_functions(Specs),
    Messages = make_messages(Functions, Target_namespace),
    Port_type = make_port_type(Service_name, Functions, Target_namespace),
    Binding = make_binding(Functions, Service_name, Target_namespace),
    Wsdl = #'wsdl:definitions'{
              targetNamespace = Target_namespace,
              types = #'wsdl:types'{
                          choice = [Types]},
              message = Messages,
              portType = [Port_type],
              binding = [Binding],
              service = [make_service(Service_name, Url, Target_namespace)]},
    Model = soap_decode_wsdl_1_1:model(),
    {ok, R} = erlsom:write(Wsdl, Model),
    Xml = erlsom_lib:prettyPrint(R),
    file:write_file(Wsdl_name, Xml).


%%% ============================================================================
%%% Internal functions
%%% ============================================================================

split_forms(Tokens) ->
    split_forms(Tokens, [], []).
split_forms([{dot, Line}  | Tail], TokenAcc, FormAcc) ->
    split_forms(Tail, [], [lists:reverse([{dot, Line} | TokenAcc]) | FormAcc]);
split_forms([], [], FormAcc) ->
    lists:reverse(FormAcc);
split_forms([Token | Tail], TokenAcc, FormAcc) ->
    split_forms(Tail, [Token | TokenAcc], FormAcc).

make_functions(Specs) ->
    lists:foldl(fun make_function/2, [], Specs).

make_function({attribute, _, spec, {Name, [Signature]}}, Acc) ->
    {type, _, 'fun', Types} = Signature,
    [{type, _, product, In_types},  Out_param] = Types,
    %% there must always be 3 parameters:
    %% 1 - the record that is specific for the function
    %% 2 - the soap_req
    %% 3 - the soap_handler_state
    [In_param, _, _] = In_types, 
    In_type = get_type(In_param),
    Out_type = get_type(Out_param),
    [#operation{name = make_name(Name), in = In_type, out = Out_type} | Acc].

make_binding(Functions, Service_name, Ns) ->
    #'wsdl:binding'{
       name = binding_name(Service_name),
       type = make_qname(port_type_name(Service_name), Ns),
       choice = [#'soap:binding'{
                   transport = "http://schemas.xmlsoap.org/soap/http"}],
       operation = make_binding_operations(Functions)}.

binding_name(Service_name) ->
    Service_name ++ "Binding".

make_binding_operations(Functions) ->
    [make_binding_operation(Op) || Op <- Functions].

make_binding_operation(#operation{name = Name}) ->
    #'wsdl:bindingOperation'{
       name = atom_to_list(Name),
       choice = [#'soap:operation'{
                   soapAction = atom_to_list(Name),
                   style = "document"}],
       input = #'wsdl:bindingOperationMessage'{
                  choice = [#'soap:body'{
                              use = "literal"}]},
       output = #'wsdl:bindingOperationMessage'{
                  choice = [#'soap:body'{
                              use = "literal"}]}}.

make_service(Service_name, Url, Ns) ->
    #'wsdl:service'{
       name = Service_name,
       port = [#'wsdl:wsdl_port'{
                 name = Service_name ++ "Soap",
                 binding = make_qname(binding_name(Service_name), Ns),
                 choice = [#'soap:address'{location = Url}]}]}.

make_port_type(Service_name, Functions, Ns) ->
    #'wsdl:portType'{
       name = port_type_name(Service_name),
       operation = make_operations(Functions, Ns)}.

port_type_name(Service_name) ->
    Service_name ++ "PortType".

make_operations(Ops, Ns) ->
    [make_operation(Op, Ns) || Op <- Ops].

make_operation(#operation{name = Name, in = In, out = Out}, Ns) ->
    #'wsdl:operation'{
       name = atom_to_list(Name),
       choice1 = #'wsdl:request-response-or-one-way-operation'{ 
                    input = make_param(In, "In", Ns),
                    output = make_param(Out, "Out", Ns)}}.

make_param(Name, Direction, Ns) ->
    #'wsdl:param'{message = make_qname(atom_to_list(Name) ++ Direction, Ns)}.

make_qname(String, Target_ns) ->
    #qname{uri = Target_ns,
           localPart = String,
           mappedPrefix = "tns",
           prefix = "tns"}.

make_messages(Functions, Ns) ->
    InMessages = [{In, "In"} || #operation{in = In} <- Functions],
    OutMessages = [{Out, "Out"} || #operation{out = Out} <- Functions],
    Messages = lists:usort(InMessages ++ OutMessages),
    [make_message(M, Ns) || M <- Messages].

make_message({Name, Postfix}, Ns) ->
    #'wsdl:message'{
      name = atom_to_list(Name) ++ Postfix,
      part = [#'wsdl:part'{name = "parameters",
                          element = make_qname(atom_to_list(Name), Ns)}]}.

get_type({ann_type,_, [{var,_,_}, Type]}) ->
    get_type(Type);
get_type({remote_type,_, [{atom,_,soap},
                          {atom,_,soap_handler_response},
                          [Type]]}) ->
    get_type(Type);
get_type({user_type, _, Type, _}) ->
    Type;
get_type({type,_,record,[{atom,_,Type}]}) ->
    Type.

make_name({Name, _Arity}) -> 
    Name.
