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

-define(SOAP_NS, "http://schemas.xmlsoap.org/soap/envelope/").
-define(SOAP12_NS, "http://www.w3.org/2003/05/soap-envelope").

-record(op, {
    name :: string(),
    operation :: atom(), %% name of function as used in handler module
    soap_action = [] :: string(),
    op_type :: notification | request_response,
    in_type :: atom(),
    out_type :: undefined | atom(),
    fault_types :: [atom()]}).
-type op() :: #op{}.

-record(interface, {
    service :: string(),
    module :: module(), %% The module that makes the interface available
    version :: '1.1' | '1.2',
    http_client :: module(),
    http_server :: module(),
    server_handler :: module(),
    client_handler :: module(),
    http_options = [] :: [any()],
    soap_ns :: string(),
    decoders :: [{string(), module}], 
    url :: string(),
    port :: string(),
    binding :: string(),
    port_type :: string(),
    ops = [] :: [op()],
    model :: erlsom:model(),
    %% the fields below are only used during the creation of the interface
    prefix_count = 0 :: integer(), %% used to assign unique prefixes
    imported = [] :: [{string(), string() | undefined}] %% imported namespaces,
    %% {URI, Prefix}, to prevent duplicates and to be able to add the 
    %% prefix later on.
}).
-type interface() :: #interface{}.
