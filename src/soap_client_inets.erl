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
%%% Support for httpc (inets) as the HTTP client that is used by the SOAP 
%%% client.
%%%
%%% This module impements the inteface between the framework and the HTTP 
%%% client. This interface requires only 1 function: http_request/5.
%%%
-module(soap_client_inets).

-export([http_request/5]).
-export([start/0]).
-export([stop/0]).

-type http_header() :: soap:http_header().
-type http_response() :: soap:http_response().

-spec start() -> any().
start() ->
    inets:start().

-spec stop() -> any().
stop() ->
    inets:stop().

-spec http_request(URL::string(), Message::iolist(), Options::any(),
                   [http_header()], Content_type::string()) -> http_response().
http_request(URL, Request, Options, Headers, ContentType) ->
    Request_as_binary = iolist_to_binary(Request),
    case httpc:request(post, {URL, Headers, ContentType, Request_as_binary},
                       Options,
                       [{body_format, binary}]) of
        {ok,{{_HTTP,200,_OK},ResponseHeaders,ResponseBody}} ->
              {ok, 200, ResponseHeaders, ResponseBody};
        {ok,{{_HTTP,500,_Descr},ResponseHeaders,ResponseBody}} ->
            {ok, 500, ResponseHeaders, ResponseBody};
        {ok,{{_HTTP,ErrorCode,_Descr},ResponseHeaders,ResponseBody}} ->
            {ok, ErrorCode, ResponseHeaders, ResponseBody};
        Other ->
            Other
    end.
