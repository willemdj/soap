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

%%% SOAP handler for the Mochiweb http server
%%%
-module(soap_server_mochiweb).

-export([start/1]).
-export([start/2]).
-export([stop/0]).
-export([handle/3]).

start(Module) ->
    start(Module, []).

start(Module, Options) ->
    Port = proplists:get_value(port, Options, 8080),
    Handle_requests = make_handler_fun(Module, Options),
    mochiweb_http:start([{port, Port},
                         {link, false},
                         {loop, Handle_requests}]).

stop() ->
    mochiweb_http:stop().

%% This is a simple example, especially if you want to run other 
%% services on the webserver besides this soap service you will
%% have to do some routing here, for example based on the URL.
make_handler_fun(Handler, Options) ->
    fun(Req) ->
        ?MODULE:handle(Req, Handler, Options)
    end.


%% This is called by the mochiweb server for each request.
handle(Req, Handler, Options) ->
    try
        {continue, Soap_req} = 
            soap_server_handler:new_req(Handler, mochiweb, Options, Req),
        Soap_req2 = enrich_req(Req, Soap_req),
        case soap_server_handler:check_http_conformance(Soap_req2) of
            {continue, Soap_req3} ->
                Req_body = Req:recv_body(),
                Soap_req4 = soap_req:set_http_body(Soap_req3, Req_body),
                Handler_resp = 
                    soap_server_handler:handle_message(Req_body, Soap_req4),
                {ok, StatusCode, Headers, Resp_body, _Req2} = Handler_resp,
                Req:respond({StatusCode, Headers, Resp_body});
            {ok, StatusCode, Headers, Resp_body, _} ->
                Req:respond({StatusCode, Headers, Resp_body})
        end
    catch
        Class:Reason ->
            io:format("Class: ~p, Reason: ~p, Stack: ~p~n", 
                      [Class, Reason, erlang:get_stacktrace()])
    end.

enrich_req(Req, Soap_req) ->
    Method = Req:get(method),
    Soap_req2 = soap_req:set_method(Soap_req, atom_to_list(Method)),
    Content_type = Req:get_header_value("content-type"),
    Soap_req3 = soap_req:set_content_type(Soap_req2, Content_type),
    Soap_action = Req:get_header_value("soapaction"),
    soap_req:set_soap_action(Soap_req3, Soap_action).
