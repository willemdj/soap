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

%%%
%%% Reads a WSDL (rather: uses module 'soap_parse_wsdl' to do that), and 
%%% writes Elang modules (stubs, skeletons and a .hrl file).
%%%
-module(soap_compile_wsdl).

-export([file/5]).
-export([get_services/2]).
-export([get_namespaces/2]).

-include("soap.hrl").

-type option() :: {http_client, atom()} | 
                  {http_options, any()} |
                  {http_server, atom()} |
                  {attachments, boolean()} |
                  {include_path, string()} |   %% for testing purposes
                  {wsdl_version, '1.1' | '2.0'} |
                  {test_values, boolean()} |
                  {generate, client | server | both}.

-type service_name() :: string().
-type port_name() :: string().
-type service_list() :: [{service_name(), [port_name()]}].
-export_type([service_list/0]).

-type uri() :: string().
-type prefix() :: string().
-type namespace_list() :: [{uri(), prefix()}].
-export_type([namespace_list/0]).

%%% ============================================================================
%%% Exported functions
%%% ============================================================================

%% Read the WSDL file and generate the Erlang code.
-spec file(File_name::string(), 
           Service::string(), Port::string(), Options::[option()], 
           Hrl_name::string()) -> ok | {error, any()}.
file(Wsdl_file, Service, Port, Options, Module) -> 
    Module_atom = list_to_atom(filename:basename(Module)),
    Generate = proplists:get_value(generate, Options, both),
    Generate_tests = proplists:get_value(generate_tests, Options, none),
    Generate_test_client = (Generate_tests == both) or 
                           (Generate_tests == client),
    Generate_test_server = (Generate_tests == both) or 
                           (Generate_tests == server),
    Make_client = (Generate == client) or (Generate == both),
    Make_server = (Generate == server) or (Generate == both),
    Client = proplists:get_value(client_name, Options, Module ++ "_client"),
    Handler = proplists:get_value(server_name, Options, Module ++ "_server"),
    %% there are separate parsers for WSDL 1.1 and 2.0
    Parser = parser_module(Options),
    Interface = Parser:file(Wsdl_file, Service, Port, Options),
    Interface2 = 
        Interface#interface{
            http_client = proplists:get_value(http_client, 
                                              Options, soap_client_ibrowse),
            http_options = proplists:get_value(http_options, Options, []),
            http_server = proplists:get_value(http_server, 
                                              Options, soap_server_cowboy_1),
            module = Module_atom},
    case Make_server of
        true ->
            Interface3 = 
                Interface2#interface{server_handler = list_to_atom(Handler)},
                write_server(Wsdl_file, Service, Port, Options, Module, 
                             Handler, Interface3);
        false ->
            Interface3 = Interface2
    end,
    case (Make_client or Generate_test_client) of
        true -> 
            Interface4 = Interface3#interface{
                           client_handler = list_to_atom(Client)},
            write_client(Wsdl_file, Service, Port, Options, Module, 
                         Client, Interface3);
        false ->
            Interface4 = Interface3
    end,
    case Generate_test_client of
      false -> ok;
      true ->
         soap_test_module:from_interface(Interface4, Client, Client ++ "_test",
                                         hrl_file_name(Module), Options)
    end,
    case Generate_test_server of
      false -> ok;
      true ->
         write_server(Wsdl_file, Service, Port, 
                      [{test_values, true} | Options], Module, 
                      Handler ++ "_test", Interface4)
    end,
    write_hrl(Interface4, Wsdl_file, Options, Module).

%% Provide a list of services and associated ports (or endPoints, in the 
%% case of WSDL 2.0). Used to be able to select a service in the dialogue
%% in wsdl2erlang. 
-spec get_services(File::string(), [option()]) -> service_list().
get_services(File, Options) ->
    Parser = parser_module(Options),
    Parser:get_services(File, Options).

-spec get_namespaces(File::string(), [option()]) -> namespace_list().
get_namespaces(File, Options) ->
    Parser = parser_module(Options),
    Parser:get_namespaces(File, Options).


%%% ============================================================================
%%% Internal functions
%%% ============================================================================

%%-----------------------------------------------------------------------------
%% Generate the hrl file with the types that are created/used by erlsom, as well 
%% as the #interface{} that is used by the SOAP framework, and a macro INTERFACE
%% to make it accessible.
%% ----------------------------------------------------------------------------
write_hrl(#interface{model = Model, service = Service, port = Port} = Interface, 
          Wsdl_file, Options, Module) ->
    Hrl_header = [info_about_command(Wsdl_file, Service, Port, Options),
                  hrl_header()],
    {_, Types} = erlsom_writeHrl:write_hrl(Model),
    Code = io_lib:format("~n~s~s~n-define(INTERFACE, ~p).~n",
                         [Hrl_header, Types, Interface]),
    Output_file = hrl_file_name(Module),
    ok = file:write_file(Output_file, Code),
    io:format("==> Generated file ~s~n", [Output_file]),
    ok.

hrl_file_name(Module) ->
    Module ++ ".hrl".

hrl_header() ->
"%%% This file contains record and type decarations that are used by the WSDL.\n" 
"%%%\n"
"%%% It also contains a macro 'INTERFACE' that is used to make information\n"
"%%% about the WSDL available to the SOAP implementation.\n"
"%%%\n"
"%%% It is possible (and in some cases necessary) to change the name of the\n"
"%%% record fields.\n"
"%%%\n"
"%%% It is possible to add default values, but be aware that these will only\n"
"%%% be used when *writing* an xml document.\n"
"\n"
"%%% Records used to represent fault response messages:\n"
"\n"
"-record(faultdetail, {uri :: string(),\n"
"                      tag :: string(),\n"
"                      text :: string()}).\n"
"\n"
"-record(faultcode, {uri :: string(),\n"
"                    code :: string(),\n"
"                    subcode :: #faultcode{} % only v. 1.2\n"
"                   }).\n"
"\n"
"-record(faultreason, {text :: string(),\n"
"                      language :: string()}).\n"
"\n"
"-record(soap_fault_1_1, {faultcode :: #faultcode{},\n"
"                         faultstring :: string(),\n"
"                         faultactor :: string(),\n"
"                         detail :: [#faultdetail{}]}).\n"
"\n"
"-record(soap_fault_1_2, {code :: #faultcode{},\n"
"                         reason :: [#faultreason{}],\n"
"                         role :: string(),\n"
"                         detail :: [#faultdetail{}]}).\n"
"\n".

%%-----------------------------------------------------------------------------
%% Generate the code: 
%% - a function that makes the infromation from the WSDL available
%% - client functions (if requested).
%% ----------------------------------------------------------------------------
header(Wsdl_file, Service, Port, Options, Hrl_name, Filename) ->
    info_about_command(Wsdl_file, Service, Port, Options) ++
    "-module(" ++ atom_string(Filename) ++ ").\n\n"
    "-include(\"" ++ hrl_file_name(Hrl_name) ++ "\").\n\n"
    "-export([interface/0]).\n\n".

info_about_command(Wsdl_file, Service, Port, Options) ->
    "%% generated by soap from: " ++ filename:absname(Wsdl_file) ++ 
    "\n%% for service \"" ++ Service ++ "\" and port \"" ++ Port ++ "\"" ++
    "\n%% using options: " ++ 
    io_lib:format("~2500p~n~n", [[{service, Service}, {port, Port} | Options]]).

write_client(Wsdl_file, Service, Port, Options, Module, Filename,
             #interface{ops = Ops}) ->
    Attachments = proplists:get_value(attachments, Options, false),
    Intro = header(Wsdl_file, Service, Port, Options, Module, Filename),
    {Client_exports, Client_functions} = make_client(Ops, Attachments),
    Model_fun = model_fun(),
    Code = io_lib:format("~s~s~s~s", 
                         [Intro, Client_exports,
                          Client_functions, Model_fun]),
    ok = file:write_file(Filename ++ ".erl", Code),
    io:format("==> Generated file ~s~n", [Filename ++ ".erl"]),
    ok.

make_client(Ops, Attachments) ->
    Client_export_header = client_export_header(),
    Client_exports = [[make_export(Op, Attachments) || Op <- Ops], "\n"],
    Operations = [make_operation(Op, Attachments) || Op <- Ops],
    {[Client_export_header, Client_exports], Operations}.

make_export(#op{name = Name}, Attachments) -> 
    Arity = case Attachments of
                true -> "/4";
                _    -> "/3"
            end,
    ["-export([", atom_string(Name), Arity, "]).\n"].

make_operation(#op{name = Name, in_type = In, out_type = Out,
                   soap_action = Action}, Attachments) -> 
    case Attachments of 
        true ->
            Att_spec = ", Attachments::[soap:soap_attachment()]",
            Att_call = ", Attachments";
        _ ->
            Att_spec = "",
            Att_call = ""
    end,
    ["-spec ", atom_string(Name), "(Soap_body::", make_arg(In), ",\n",
     "  Soap_headers::[soap:soap_header()],\n",
     "  Options::[any()]", Att_spec, ") -> ", make_client_result(Out), ".\n",
     atom_string(Name), "(Soap_body, Soap_headers, Options",
     Att_call, ") ->\n", 
     "  soap_client_util:call(Soap_body, Soap_headers, Options, \"\\\""
     ++ Action ++ "\\\"\", interface()", Att_call, ").\n\n"].

make_arg(Type) ->
    io_lib:format("~p()", [Type]).

make_result(undefined) ->
    "soap:soap_handler_response([])";
make_result(Type) ->
    io_lib:format("soap:soap_handler_response(~p())", [Type]).

make_client_result(undefined) ->
    "soap:soap_response([])";
make_client_result(Type) ->
    io_lib:format("soap:soap_response(~p())", [Type]).

client_export_header() ->
    "%% The functions that are described by the WSDL\n".

model_fun() ->
    "%%% --------------------------------------------------------------------\n"
    "%%% Internal functions\n"
    "%%% --------------------------------------------------------------------\n"
    "interface() ->\n"
    "    ?INTERFACE.".

%%% --------------------------------------------------------------------
%%% The _server file with stubs.
%%% --------------------------------------------------------------------
write_server(Wsdl_file, Service, Port, Options, Module, Handler, 
             #interface{ops = Ops} = Interface) ->
    Example_value = proplists:get_value(test_values, Options, false),
    Intro = server_header(Wsdl_file, Service, Port, Options, Handler, Module),
    Exported_operations = exported_operations(Ops),
    Exports = server_exports(),
    Operations = server_ops(Ops, Interface, Example_value),
    Code = [Intro, Exported_operations, Exports, Operations,
            internal()],
    Filename = Handler ++ ".erl",
    ok = file:write_file(Filename, Code),
    io:format("==> Generated file ~s~n", [Filename]).

server_header(Wsdl_file, Service, Port, Options, Handler, Module) ->
    info_about_command(Wsdl_file, Service, Port, Options) ++
    "-module(" ++ atom_string(Handler) ++ ").\n\n"
    "-include(\"" ++ hrl_file_name(Module) ++ "\").\n\n".

server_exports() ->
    "-export([interface/0]).\n\n".

internal() ->
"%% The 'interface()' function is used by the SOAP framework to access information about\n"
"%% the WSDL.\n" 
"interface() ->\n"
"    ?INTERFACE.\n".

exported_operations(Ops) ->
    [exported_operation(Op) || Op <- Ops].

exported_operation(#op{name = Name}) ->
    ["-export([", atom_string(Name), "/3]).\n"].

server_ops(Ops, Interface, Example_value) ->
    [server_operation(Op, Interface, Example_value) || Op <- Ops].

server_operation(#op{name = Name, in_type = In, out_type = Out},
                 #interface{model = Model}, Example_value) -> 
    ["-spec ", atom_string(Name), "(Parsed_body::", make_arg(In), ",\n",
     "    Soap_req::soap:soap_req(), State::soap:soap_handler_state())\n",
     "    -> ", make_result(Out), ".\n",
     atom_string(Name), "(_Parsed_body, Soap_req, State) ->\n", 
     case Example_value of
         true ->
             ["    Result = \n" ,
              erlsom_example_value:from_model(Out, Model, [{indent_level, 2}]),
              ",\n    {ok, Result, Soap_req, State}.\n\n"];
         false ->
             ["    %% your implementation goes here\n",
              io_lib:format("    {ok, #~p{}, Soap_req, State}.\n\n", [Out])]
     end
    ].

%% example: 
%% "SendMessage" -> "'SendMessage'"
%% "sendMessage" -> "sendMessage"
atom_string(String) ->
    Atom = list_to_atom(String),
    [Atom_string] = io_lib:format("~p", [Atom]),
    Atom_string.

parser_module(Options) ->
    case proplists:get_value(wsdl_version, Options, '1.1') of
        '1.1' ->
            soap_parse_wsdl;
        '2.0' ->
            soap_parse_wsdl_2_0
    end.
