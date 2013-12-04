-module(ws1_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {width=100, height=80, move_number=0, plants_percent=10}).

init({tcp, http}, _Req, _Opts) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
    erlang:start_timer(1000, self(), <<"ants server 0.0.1">>),
    {ok, Req, #state{}}.

websocket_handle({text, <<"init map ", R/binary>>}, Req, State) ->
    lager:debug("State: ~p", [State]),
    world_data_handler:reload(),
    [W,H,PlantsPercents] = [list_to_integer(X)||X<-re:split(R, ":", [{return, list}])],
    CreatureID = world_data_handler:world_insert(<<"b1">>, 1, 1, 0),
    world_data_handler:recents_insert(<<"b1">>, 0, 1),
    {reply, {text, world_data_handler:init_map(W,H,[{<<"plant">>, PlantsPercents}], [{CreatureID, <<"b1">>, 0, 1}])}, Req, 
        State#state{width=W, height=H, plants_percent=PlantsPercents}};
websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, #state{move_number=MN} = State) ->
    lager:debug("WST: ~p", [Msg]),
    erlang:start_timer(1000, self(), <<"next_move">>),
    {reply, {text, gen_move_message(State)}, Req, State#state{move_number=MN+1}};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%gen_move_message(#state{width = W} = State) -> 
%    jiffy:encode([[N,P,new_position(N,M, {P div W, P rem W}, State)] || {N,_,P,M} <- ets:tab2list(world_tab)]).

gen_move_message(#state{width = W, height = H} = State) ->
    Beasts = world_data_handler:get_beasts(),
    jiffy:encode([[Id, T, P, new_position(P, W, H, B, State, {Id, T})] || {Id, T, _, P, _} = B <- Beasts]).

new_position(Pos, Width, Height, _, #state{move_number = MN}, {BeastId, BeastType}) ->
    {R, C} = {Pos div Width, Pos rem Width}, 
    OldPos = R*Width + C,
    Fun = fun(CPos) ->
        case world_data_handler:check_position(CPos) of
            [{CPos, Id, <<"plant">>}] -> {CPos, Id, <<"plant">>}
            ;_ -> []
        end
    end, 
    NewPos = case mants_defs:get_a_look(R,C,Width,Height,3,Fun) of
        [{Head, _TId, <<"plant">>}|_] -> 
            %goto_target(Head, {N,M,OldPos,{R,C,Width,Height},MN})
            world_data_handler:goto_target(Head, Width, R, C)
        ;_    ->
                StopPos = world_data_handler:get_recents_positions(BeastType, R, C, Width),
                PossPos = world_data_handler:get_possible_positions(BeastType, R, C, Width, Height),
                lager:debug("Stop,Poss: ~p ~p", [StopPos, PossPos]),
                case PossPos -- StopPos of
                    [] -> world_data_handler:get_oldest_sibling(PossPos,R,C,Width,OldPos,BeastType);
                    L  -> lists:nth(random:uniform(length(L)), L)
                end
    end,
    lager:debug("NP ~p ~p ~p", [MN, OldPos, NewPos]),
    world_data_handler:process_new_position(BeastId, BeastType, NewPos, MN, OldPos, M),
    NewPos.
