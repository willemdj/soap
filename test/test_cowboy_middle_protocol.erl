-module(test_cowboy_middle_protocol).

-export([start/1]).
-export([start/2]).
-export([stop/0]).
-export([init/3, init/2]).
-export([on_request/1]).
-export([upgrade/4, upgrade/6]).

start(Module) ->
  start(Module, []).

start(Module, Options) ->
  
  Port = proplists:get_value(port, Options, 8080),
  Acceptors = proplists:get_value(nr_acceptors, Options, 100),
  {ok, _} = application:ensure_all_started(cowboy),
  Dispatch = cowboy_router:compile([
                                    {'_', [{'_', ?MODULE, {Module, Options}}]}]),
  {ok, _} = cowboy:start_http(http, Acceptors, [{port, Port}], 
                              [ {env, [{dispatch, Dispatch}]},
                                {onrequest, fun ?MODULE:on_request/1}
                              ]).

stop() ->
  cowboy:stop_listener(http),
  application:stop(cowboy), 
  application:stop(ranch).

on_request(Req0) ->
  {ok, Body, Req1} = cowboy_req:body(Req0),
  cowboy_req:set_meta(body, Body, Req1).
    
%% cowboy 1 callback
init(_, _Req, {_Module, Options}) ->
  %% The module 'soap_cowboy_protocol' will be called
  %% for each request, with Module (= the handler module) and 
  %% the options as parameter. 
  soap_server_cowboy_1 = proplists:get_value(cowboy_version, Options),
  {upgrade, protocol, ?MODULE}.

%% cowboy 1 callback
upgrade(Cowboy_req, Env, Soap_handler, {Handler, Options}) ->
  Options2 = proplists:delete(cowboy_version, Options),
  {Message, Cowboy_req2} = cowboy_req:meta(body, Cowboy_req),

  soap_cowboy_1_protocol:upgrade(
    Cowboy_req2, Env, Soap_handler, {Handler, Options2}, Message).

%% cowboy 2 callback
init(Req, {Module, Options}) ->
  soap_server_cowboy_2 = proplists:get_value(cowboy_version, Options),
  {?MODULE, Req, {Module, Options}}.

%% cowboy 2 callback
upgrade(Cowboy_req, Env, Soap_handler, {Handler, Options}, Timeout, Hibernate) ->
  Options2 = proplists:delete(cowboy_version, Options),
  {Message, Cowboy_req2} = cowboy_req:meta(body, Cowboy_req),

  soap_cowboy_2_protocol:upgrade(
    Cowboy_req2, Env, Soap_handler, {Handler, Options2}, Timeout, Hibernate, Message).
