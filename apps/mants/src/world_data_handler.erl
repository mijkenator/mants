-module(world_data_handler).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%API
-export([
    world_insert/4,
    recents_insert/3,
    init_map/4,
    get_beasts/0,
    reload/0,
    check_position/1,
    get_recents_positions/4,
    get_possible_positions/5,
    get_oldest_sibling/6,
    goto_target/4,
    delete_object/1,
    process_new_position/6
]).

-record(state, {
    current_creature_id = 0,
    inited = 0
    }).

-include("include/creatures.hrl").
-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%
% wdt_world_tab {creature_id, creature_type, old_pos, new_pos, mode}
% wdt_recents_tab {creature_type, move_number, pos}
% wdt_geo_tab {creature_type, pos}
%
% wdt_map_lookup {pos, creature_id, creature_type}
%

init([]) ->
  lager:debug("WDT INIT!", []),
  ets:new(wdt_world_tab,   [protected, named_table, set]), % creatures positions
  ets:new(wdt_recents_tab, [protected, named_table, bag]), % creatures recent positions (for explore mode)
  ets:new(wdt_geo_tab,     [protected, named_table, bag]), % plants, water, rocks ....  positions
  ets:new(wdt_map_lookup,  [protected, named_table, bag]), % 
  {ok, #state{}}.

handle_call({delete_object, {Type, Pos}}, _, State) ->
    ets:delete_object(wdt_geo_tab, {Type, Pos}),
    lists:foreach(
        fun({_,_,GT} = E) when GT =:= Type -> ets:delete_object(E)
           ;(_) -> ok
        end,
        ets:lookup(wdt_map_lookup, Pos)),
    {reply, ok, State};
handle_call({set_inited, I}, _, State) ->
    {reply, ok, State#state{inited=I}};
handle_call(is_inited, _, #state{inited=I} = State) ->
    {reply, I, State};
handle_call(reload, _, State) ->
    ets:delete_all_objects(wdt_world_tab),    
    ets:delete_all_objects(wdt_recents_tab),    
    ets:delete_all_objects(wdt_geo_tab),    
    ets:delete_all_objects(wdt_map_lookup),    
    {reply, ok, State#state{current_creature_id=0, inited=0}};
handle_call(get_new_creature_id, _, #state{current_creature_id=CCI} = State) ->
    NCCI = CCI + 1,
    {reply, NCCI, State#state{current_creature_id=NCCI}};
handle_call({insert_geo, Pos, Id, GeoType}, _, State) ->
    ets:insert(wdt_map_lookup, {Pos, Id, GeoType}),
    ets:insert(wdt_geo_tab,    {GeoType, Pos}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({etsi, Tab, Value}, State) ->
  ets:insert(Tab, Value),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% API funcs
%

reload() -> gen_server:call(?SERVER, reload).

delete_object({Type, Pos}) -> gen_server:call(?SERVER, {delete_object, {Type, Pos}}).

-spec world_insert(binary(), integer(), integer(), integer()) -> integer().
world_insert(CreatureType, OldPos, NewPos, Mode) ->
    CreatureID = get_new_creature_id(), 
    lager:debug("New ID: ~p ~p", [CreatureID, {CreatureID, CreatureType, OldPos, NewPos, Mode}]),
    %ets:insert(wdt_world_tab, {CreatureID, CreatureType, OldPos, NewPos, Mode}),
    gen_server:cast(?SERVER, {etsi, wdt_world_tab, {CreatureID, CreatureType, OldPos, NewPos, Mode}}),
    CreatureID.

-spec get_new_creature_id() -> integer().   
get_new_creature_id() ->
    gen_server:call(?SERVER, get_new_creature_id).

-spec recents_insert(binary(), integer(), integer()) -> ok.
recents_insert(CreatureType, MoveNumber, Pos) ->
    %ets:insert(wdt_recents_tab, {CreatureType, MoveNumber, Pos}),
    gen_server:cast(?SERVER, {etsi, wdt_recents_tab, {CreatureType, MoveNumber, Pos}}),
    ok.

-spec init_map(integer(), integer(), init_p_list(), crp_list()) -> binary().
init_map(Width, Height, InitPercents, ExtraCreatureList) ->
    case already_inited() of
        _ ->
            set_inited(1),
            jiffy:encode([init_m_internal(ExtraCreatureList, {Width, Height}) | 
                  init_m_internal(mants_defs:init_merger(InitPercents), {Width, Height})])
    end.

already_inited() -> gen_server:call(?SERVER, is_inited).

set_inited(I)    -> gen_server:call(?SERVER, {set_inited, I}).

init_m_internal(L, {Width, Height}) ->
    Fun = fun(E) -> creature_init_process(E, {Width, Height}) end,
    [tuple_to_list(X) || X <- lists:flatten(lists:map(Fun, L))].

creature_init_process({CreatureID, CreatureType, OldPos, Pos}, _) ->
    {CreatureID, CreatureType, OldPos, Pos};    
creature_init_process({GeoType, Percent}, {W, H}) ->
    Count = (W*H*Percent) div mants_defs:percent_div(),
    [gen_geo(W,H,GeoType) || _ <- lists:seq(1, Count)].

-spec gen_geo(integer(), integer(), binary()) -> {integer(), binary(), integer()}.
gen_geo(W, H, GeoType) when GeoType =:= <<"rock">>; GeoType =:= <<"water">> ->
    %Pos = get_unused_pos(W, H, GeoType),
    Pos = case random:uniform(100) of
        Var when Var < 10 -> get_unused_pos(W, H)
        ;_ -> get_unused_pos(W, H, GeoType)
    end,
    ID  = get_new_creature_id(),
    gen_server:call(?SERVER, {insert_geo, Pos, ID, GeoType}),
    {ID, GeoType, Pos};
gen_geo(W, H, GeoType) ->
    Pos = get_unused_pos(W, H),
    ID  = get_new_creature_id(),
    gen_server:call(?SERVER, {insert_geo, Pos, ID, GeoType}),
    {ID, GeoType, Pos}.

-spec get_unused_pos(integer(), integer()) -> integer().
get_unused_pos(W, H) ->
    Pos = random:uniform(W*H-1),
    case ets:lookup(wdt_map_lookup, Pos) of
        [] -> Pos
        ;_ -> get_unused_pos(W, H)
    end.

-spec get_unused_pos(integer(), integer(), binary()) -> integer().
get_unused_pos(W, H, GeoType) ->
    case ets:lookup(wdt_geo_tab, GeoType) of
        [] -> get_unused_pos(W, H)
        ;L -> get_neibhour_pos(W, H, GeoType, L)
    end.

get_neibhour_pos(W, H, _GeoType, [])          -> get_unused_pos(W, H);
get_neibhour_pos(W, H, GeoType, [_Hd|Tl] = L) ->
    Pos = element(2,lists:nth(random:uniform(length(L)), L)),  
    PP  = mants_defs:get_possible_positions(Pos div W, Pos rem W, W, H),
    case choose_pos(PP) of
        0 -> get_neibhour_pos(W, H, GeoType, Tl);
        P -> P
    end.

choose_pos([])    -> 0;
choose_pos([H|T]) ->
    case ets:lookup(wdt_map_lookup, H) of
        [] -> H
        ;_ -> choose_pos(T)
    end.


get_beasts() -> ets:tab2list(wdt_world_tab).

check_position(Pos) -> ets:lookup(wdt_map_lookup, Pos).

get_recents_positions(BeastType, R, C, W) ->
    case ets:lookup(wdt_recents_tab, BeastType) of    
        [] -> [];
        L  -> lists:usort([Rp||{_,_,Rp}<-lists:filter(fun({_,_,E})->
                {Cr, Cc} = {E div W, E rem W},
                case {abs(R-Cr), abs(C-Cc)} of
                    {A1,A2} when A1<2,A2<2 -> true
                    ;_                     -> false
                end
              end, L)])
    end.

get_possible_positions(_BeastType, R, C, W, H) ->
    Fun = fun(Pos) ->
        case check_position(Pos) of
            [] -> true
            ;_ -> false
        end
    end,
    lists:filter(Fun, mants_defs:get_possible_positions(R, C, W, H)).

get_oldest_sibling(PossPos,R,C,Width,CurrentPosition,BeastType) ->
    case ets:lookup(wdt_recents_tab, BeastType) of    
        [] -> lists:nth(random:uniform(length(PossPos)), PossPos);
        L  -> 
              Fun = fun({_,A,_}, {_,B,_}) -> A =< B end,
              SF = lists:sort(Fun, lists:filter(fun({_,_,E})->
                {Cr, Cc} = {E div Width, E rem Width},
                case {abs(R-Cr), abs(C-Cc)} of
                    {A1,A2} when A1<2,A2<2 -> true
                    ;_                     -> false
                end
              end, lists:usort(L))),
              lists:nth(1, [Rp||{_,_,Rp}<-SF, Rp =/= CurrentPosition])
    end.
   
goto_target(TPos, Width, R, C) ->
    {Tr, Tc} = {TPos div Width, TPos rem Width},
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
    NR*Width + NC.

process_new_position(BeastId, BeastType, NewPos, MoveNumber, OldPos, Mode) ->
    lists:foreach(fun({_,<<"plant">>} = E)->
        _ID = world_data_handler:world_insert(BeastType, NewPos, NewPos, 0),
        world_data_handler:recents_insert(BeastType, MoveNumber, NewPos),
        world_data_handler:delete_object(E)
    ;(_) -> ok end, ets:lookup(wdt_geo_tab, NewPos)),
    gen_server:cast(?SERVER, {etsi, wdt_world_tab, {BeastId, BeastId, OldPos, NewPos, Mode}}).

