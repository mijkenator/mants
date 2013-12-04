-module(mants_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/", cowboy_static, {file, "../../static/i.html"}},
                    {"/websocket", ws_handler, []},
                    {"/ws1", ws1_handler, []},
                    %{"/static/[...]", cowboy_static, {priv_dir, websocket, "../../static"}}
                    {"/[...]", cowboy_static, {dir, "../../static"}}
            ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    mants_sup:start_link().

stop(_State) ->
    ok.
