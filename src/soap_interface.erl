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

%%% Some functions to access the interface() type.
%%%
%%% The interface type is used by the SOAP framework to capture information
%%% from the WSDL.
%%%
-module(soap_interface).

-include("soap.hrl").

-export([model/1]).
-export([handler/1]).
-export([client_handler/1]).
-export([operations/1]).
-export([http_server/1]).

-spec model(interface()) -> erlsom:model().
model(#interface{model = Model}) ->
    Model.

-spec handler(interface()) -> module().
handler(#interface{server_handler = Handler}) ->
    Handler.

-spec client_handler(interface()) -> module().
client_handler(#interface{client_handler = Handler}) ->
    Handler.

-spec operations(interface()) -> [#op{}].
operations(#interface{ops = Operations}) ->
    Operations.

-spec http_server(interface()) -> module().
http_server(#interface{http_server = Server}) ->
    Server.
