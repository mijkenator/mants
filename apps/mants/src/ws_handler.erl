-module(ws_handler).
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

%
% world_tab {name, old_pos, new_pos, mode}
% mode: 0 - explore, pursuit
%

%
% recents_tab {name, move_number, position}
%

websocket_init(_TransportName, Req, _Opts) ->
    ets:new(world_tab,   [protected, named_table, set]), % creatures positions
    ets:new(recents_tab, [protected, named_table, bag]), % creatures recent positions (for explore mode)
    ets:new(geo_tab,     [protected, named_table, set]), % plants, water, rocks ....  positions
    erlang:start_timer(1000, self(), <<"ants server 0.0.1">>),
    {ok, Req, #state{}}.

websocket_handle({text, <<"init map ", R/binary>>}, Req, State) ->
    lager:debug("State: ~p", [State]),
    [W,H,PlantsPercents] = [list_to_integer(X)||X<-re:split(R, ":", [{return, list}])],
    ets:insert(world_tab, {<<"beast1">>, 1, 1, 0}),
    ets:insert(recents_tab, {<<"beast1">>, 0, 1}),
    {reply, {text, jiffy:encode([[<<"beast1">>,1,1] | gen_plants(W,H,PlantsPercents)])}, Req, 
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

gen_plants(W,H,PP) ->
    Count = (W*H*PP) div 1000,
    [gen_plant(W,H)||_<-lists:seq(1,Count)].
gen_plant(W,H) ->
    Pos = random:uniform(W*H-1),
    ets:insert(geo_tab, {Pos, <<"plant">>}),
    [<<"plant">>, Pos].

gen_move_message(#state{width = W} = State) -> 
    jiffy:encode([[N,P,new_position(N,M, {P div W, P rem W}, State)] || {N,_,P,M} <- ets:tab2list(world_tab)]).

new_position(N,M, {R,C}, #state{width=W, height=H, move_number=MN} = _State) ->
    OldPos = R*W + C,
    NewPos = case get_a_look(R,C,W,H,3) of
        [{Head, <<"plant">>}|_] -> 
            goto_target(Head, {N,M,OldPos,{R,C,W,H},MN})
        ;_    ->
                StopPos = get_recents_positions({N,M,OldPos,{R,C,W,H},MN}),
                PossPos = get_possible_positions({N,M,OldPos,{R,C,W,H},MN}),
                lager:debug("Stop,Poss: ~p ~p", [StopPos, PossPos]),
                case PossPos -- StopPos of
                    [] -> get_oldest_sibling(N,PossPos,R,C,W, OldPos);
                    L  -> lists:nth(random:uniform(length(L)), L)
                end
    end,
    lager:debug("NP ~p ~p ~p", [MN, OldPos, NewPos]),
    process_new_position(N, NewPos),

    ets:insert(world_tab, {N, OldPos, NewPos, M}),
    ets:insert(recents_tab, {N, MN, NewPos}),
    NewPos.

process_new_position(<<"beast",_/binary>> = _Name, Pos) ->
    lists:foreach(fun({_,<<"plant">>} = E)->
        BN = list_to_binary(integer_to_list(random:uniform(1000000))),
        ets:insert(world_tab, {<<"beast",BN/binary>>, Pos, Pos, 0}), 
        ets:delete_object(geo_tab, E);(_) -> ok end, ets:lookup(geo_tab, Pos)).

goto_target(TPos, {_N,_M,_OldPos,{R,C,W,_H},_MN}) ->
    {Tr, Tc} = {TPos div W, TPos rem W},
    NR = if
        R > Tr -> R-1;
        R < Tr -> R+1;
        true   -> R
    end,
    NC = if
        C > Tc -> C-1;
        C < Tc -> C+1;
        true   -> C
    end,
    NR*W + NC.

get_oldest_sibling(Name,PossPos,R,C,W, CurrentPosition) ->
    case ets:lookup(recents_tab, Name) of    
        [] -> lists:nth(random:uniform(length(PossPos)), PossPos);
        L  -> 
              Fun = fun({_,A,_}, {_,B,_}) -> A =< B end,
              %lists:nth(1, [Rp||{_,_,Rp}<-lists:sort(Fun, lists:filter(fun({_,_,E})->
              %  {Cr, Cc} = {E div W, E rem W},
              %  case {abs(R-Cr), abs(C-Cc)} of
              %      {A1,A2} when A1<2,A2<2 -> true
              %      ;_                     -> false
              %  end
              %end, lists:usort(L))), Rp =/= CurrentPosition])
              SF = lists:sort(Fun, lists:filter(fun({_,_,E})->
                {Cr, Cc} = {E div W, E rem W},
                case {abs(R-Cr), abs(C-Cc)} of
                    {A1,A2} when A1<2,A2<2 -> true
                    ;_                     -> false
                end
              end, lists:usort(L))),
              lists:nth(1, [Rp||{_,_,Rp}<-SF, Rp =/= CurrentPosition])
    end.

get_recents_positions({Name, _Mode, _CurrentPosition, {R,C,W,_H}, _MoveNumber}) ->
    case ets:lookup(recents_tab, Name) of    
        [] -> [];
        L  -> lists:usort([Rp||{_,_,Rp}<-lists:filter(fun({_,_,E})->
                {Cr, Cc} = {E div W, E rem W},
                case {abs(R-Cr), abs(C-Cc)} of
                    {A1,A2} when A1<2,A2<2 -> true
                    ;_                     -> false
                end
              end, L)])
    end.

get_possible_positions({_Name, _Mode, _CurrentPosition, {R,C,W,H}, _MoveNumber}) ->
    [(R+Dr)*W+C+Dc || Dr<-gpp(R,H),Dc<-gpp(C,W), {Dr, Dc} =/= {R, C}].

gpp(0, _)                -> [0, 1];
gpp(R, H) when R=:=H-1   -> [0, -1];
gpp(R, H) when R>0,R<H-1 -> [-1, 0, 1].

get_a_look(R,C,W,H,Radius) ->
    lists:flatten([check_field((R+Dr)*W+C+Dc) || Dr<-gpp(R,H,Radius),Dc<-gpp(C,W,Radius), {Dr, Dc} =/= {R, C}]).
    
%
% need to test it
%
gpp(0, _, Rad)                      -> lists:seq(0, Rad);
gpp(R, H, Rad) when R=:=H-1         -> lists:seq(-1*Rad, 0);
gpp(R, H, Rad) when R>Rad,R<H-Rad-1 -> lists:seq(-1*Rad, Rad);
gpp(R, _, Rad) when R=<Rad          -> lists:seq(-1*R, Rad);
gpp(R, H, Rad) when R>=H-Rad-1      -> lists:seq(-1*Rad, H-1-R).

check_field(Pos) ->
    case ets:lookup(geo_tab, Pos) of
        [{Pos, <<"plant">>}] -> {Pos, <<"plant">>}
        ;_ -> []
    end.
