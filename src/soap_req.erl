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
%%% Functions to create and modify the information about a soap request.
%%%
%%% Also translates the information collected in the SOAP req to the 
%%% HTTP response.
%%%
-module(soap_req).
-include("soap.hrl").

%% public interface
-export([server_req/1]).
-export([set_server_req/2]).
-export([soap_version/1]).
-export([method/1]).
-export([server/1]).
-export([content_type/1]).
-export([soap_action/1]).
-export([mime_headers/1]).
-export([req_attachments/1]).
-export([http_body/1]).
-export([set_resp_attachments/2]).
-export([set_resp_http_headers/2]).

%% used by other modules of the soap application
-export([new/3]).
-export([set_soap_version/2]).
-export([set_method/2]).
-export([set_content_type/2]).
-export([set_soap_action/2]).
-export([set_mime_headers/2]).
-export([set_http_body/2]).
-export([operations/1]).
-export([model/1]).
-export([set_resp/2]).
-export([http_response/1]).
-export([handler/1]).
-export([handler_state/1]).
-export([set_handler_state/2]).
-export([set_resp_status_code/2]).
-export([set_req_attachments/2]).
-export([resp_status_code/1]).
-export([xml_string/1]).

-type http_status_code() :: soap:http_status_code().
-type http_body() :: iodata().
-export_type([http_body/0]).

-type http_header() :: soap:http_header().
-type server_req() :: soap:server_req().
-type server_http_response() :: soap:server_http_response().

%% Note: with the proper type definition dialyzer takes forever.
%%-type soap_handler_response() :: soap:soap_handler_response(any()).
-type soap_handler_response() :: any().
-type soap_version() :: undefined | '1.1' | '1.2'.

-type soap_attachment() :: soap:soap_attachment().

-record(soap_req, {
    soap_version :: soap_version() | undefined,
    content_type :: string() | undefined,
    method :: string() | undefined,
    soap_action :: string() | undefined,
    server_req :: server_req(),
    handler :: module(),
    handler_state :: any(),
    http_body :: binary(),
    server :: atom(),
    req_soap_attachments = [] :: [soap_attachment()],
    mime_headers = [] :: [http_header()],   %% headers of the mime part 
                                            %% with the xml message.
    resp_soap_attachments = [] :: [soap_attachment()],
    resp_status_code = undefined :: integer() | undefined,
    resp_http_headers = [] :: [http_header()],
    resp :: soap_handler_response()
}).
-opaque soap_req() :: #soap_req{}.
-export_type([soap_req/0]).


%%% ============================================================================
%%% Exported functions
%%% ============================================================================

%% Create a soap_req
-spec new(Server::atom(), Server_req::any(), Handler::module()) -> soap_req().
new(Server, Server_req, Handler) ->
    #soap_req{server_req = Server_req,
              server = Server,
              handler = Handler}.
-spec operations(soap_req()) -> [#op{}].
operations(#soap_req{handler = Handler}) ->
    soap_interface:operations(Handler:interface()).

%% Generates a server_http_response() from the information that has been 
%% collected in the soap_req() during the handling of the request.
%%
%% If it is required to explicitly set HTTP headers or the HTTP status code,
%% these can be set via soap_req:set_resp_http_headers() or 
%% soap_req:set_resp_status_code().
-spec http_response(soap_req()) -> server_http_response().
http_response(#soap_req{resp_status_code = Code,
                        soap_version = Version,
                        resp_http_headers = HTTP_headers, 
                        resp_soap_attachments = Attachments,
                        resp = Handler_response, 
                        server_req = Server_req
                        }) ->
    Status_code = 
        case Code of
            undefined ->
                result_code(Handler_response);
            _ ->
                Code
        end,
    Message_body = result_body(Handler_response, Version),
    Content_type = 
        case Version of 
            _ when Version == '1.1'; Version == undefined ->
                "text/xml";
            '1.2' ->
                "application/soap+xml"
        end,
    case Attachments of
        [] ->
            Headers = add_if_not_present(HTTP_headers, {"Content-Type", 
                                                        Content_type}),
            HTTP_body = Message_body;
        _ ->
            Headers = [{"Content-Type", soap_mime:mime_content_type()}],
            Body_headers = [{"Content-Type", Content_type}],
            HTTP_body = soap_mime:mime_body(Message_body, Body_headers, Attachments)
    end,
    {ok, Status_code, Headers, HTTP_body, Server_req}.

%%%
%%% The rest of the exported functions are all simple "setters" and "getters".
%%%

-spec handler(soap_req()) -> module().
handler(#soap_req{handler = Handler}) ->
    Handler.

-spec model(soap_req()) -> erlsom:model().
model(#soap_req{handler = Handler}) ->
    soap_interface:model(Handler:interface()).

-spec http_body(soap_req()) -> binary().
http_body(#soap_req{http_body = V}) ->
  V.

-spec set_soap_action(soap_req(), Soap_action::string()) -> soap_req().
set_soap_action(Soap_req, Soap_action) ->
    Soap_req#soap_req{soap_action = Soap_action}.

-spec soap_action(soap_req()) -> string() | undefined.
soap_action(#soap_req{soap_action = Soap_action}) ->
    Soap_action.

-spec set_http_body(soap_req(), Body::binary()) -> soap_req().
set_http_body(Soap_req, V) ->
    Soap_req#soap_req{http_body = V}.

-spec set_mime_headers(soap_req(), Headers::[http_header()]) -> soap_req().
set_mime_headers(Soap_req, V) ->
    Soap_req#soap_req{mime_headers = V}.

-spec mime_headers(soap_req()) -> [http_header()].
mime_headers(#soap_req{mime_headers = V}) ->
    V.

-spec set_content_type(soap_req(), Content_type::string()) -> soap_req().
set_content_type(Soap_req, Content_type) ->
    Soap_req#soap_req{content_type = Content_type}.

-spec content_type(soap_req()) -> string() | undefined.
content_type(#soap_req{content_type = Content_type}) ->
    Content_type.

-spec set_method(soap_req(), Method::string()) -> soap_req().
set_method(Soap_req, Method) ->
    Soap_req#soap_req{method = Method}.

-spec method(soap_req()) -> string() | undefined.
method(#soap_req{method = Method}) ->
    Method.

-spec server(soap_req()) -> atom() | undefined.
server(#soap_req{server = V}) ->
    V.

-spec set_soap_version(soap_req(), soap_version()) -> soap_req().
set_soap_version(Soap_req, Soap_version) ->
    Soap_req#soap_req{soap_version = Soap_version}.

-spec soap_version(soap_req()) -> soap_version().
soap_version(#soap_req{soap_version = Soap_version}) ->
    Soap_version.

-spec set_handler_state(soap_req(), any()) -> soap_req().
set_handler_state(Soap_req, Handler_state) ->
    Soap_req#soap_req{handler_state = Handler_state}.

-spec handler_state(soap_req()) -> any().
handler_state(#soap_req{handler_state = Handler_state}) ->
    Handler_state.

-spec server_req(soap_req()) -> any().
server_req(#soap_req{server_req = C_req}) ->
    C_req.

-spec set_server_req(soap_req(), any()) -> soap_req().
set_server_req(Soap_req, S_req) ->
    Soap_req#soap_req{server_req = S_req}.

-spec set_resp_status_code(soap_req(), http_status_code()) -> soap_req().
set_resp_status_code(S_req, Code) when is_integer(Code) ->
    S_req#soap_req{resp_status_code = Code}.

-spec set_resp_http_headers(soap_req(), [http_header()]) -> soap_req().
set_resp_http_headers(S_req, Headers) when is_list(Headers) ->
    S_req#soap_req{resp_http_headers = Headers}.

-spec req_attachments(soap_req()) -> [soap_attachment()].
req_attachments(#soap_req{req_soap_attachments = V}) ->
    V.

-spec set_req_attachments(soap_req(), [soap_attachment()]) -> soap_req().
set_req_attachments(S_req, Attachments) ->
    S_req#soap_req{req_soap_attachments = Attachments}.

-spec set_resp_attachments(soap_req(), [soap_attachment()]) -> soap_req().
set_resp_attachments(S_req, Attachments) ->
    S_req#soap_req{resp_soap_attachments = Attachments}.

-spec set_resp(soap_handler_response(), soap_req()) -> soap_req().
set_resp(Soap_resp, S_req) ->
    S_req#soap_req{resp = Soap_resp}.

resp_status_code(#soap_req{resp_status_code = Code}) ->
    Code.

%%% ============================================================================
%%% Internal functions
%%% ============================================================================

result_code({raw, Code, _, _, _}) ->
    Code;
result_code({error, Code, _, _}) ->
    Code;
result_code(Tuple) when is_tuple(Tuple) ->
    case element(1, Tuple) of
        ok ->
            200;
        fault ->
            500
    end.

-spec result_body(soap_handler_response(), soap_version()) -> iodata().
result_body({Result, Body, Headers, _, _}, Version) when Result == ok; Result == fault ->
    http_body(Body, Headers, Version);
result_body({Result, Body, _, _}, Version) when Result == ok; Result == fault ->
    http_body(Body, [], Version);
result_body({raw, _, Binary, _, _}, _Version) ->
    Binary;
result_body({error, _, _, _}, _Version) ->
    [].

http_body(Body, Headers, Version) ->
    soap_env(Body, Headers, Version).

soap_env(Body, Headers, '1.2') ->
    [<<"<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://www.w3.org/2003/05/soap-envelope\">">>,
     headers(Headers),
     <<"<SOAP-ENV:Body>">>,
     Body,
     <<"</SOAP-ENV:Body></SOAP-ENV:Envelope>">>];
%% version 1.1 is the default
soap_env(Body, Headers, _) ->
    [<<"<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">">>,
     headers(Headers),
     <<"<SOAP-ENV:Body>">>,
     Body,
     <<"</SOAP-ENV:Body></SOAP-ENV:Envelope>">>].

headers([]) ->
    [];
headers(Headers) ->
    [<<"<SOAP-ENV:Header>">>, Headers, <<"</SOAP-ENV:Header>">>].


add_if_not_present([], Tuple) ->
    [Tuple];
add_if_not_present(List, {Key, _} = Tuple) ->
    case proplists:get_value(Key, List) of
        undefined ->
            [Tuple | List];
        _ ->
            List
    end.
    
%% TODO: look at this _and_ erlsom_lib:xmlString - this is quite inefficient.
xml_string(IO_list) ->
  erlsom_lib:xmlString(unicode:characters_to_binary(IO_list)).
