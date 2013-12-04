-module(mants_defs).
-export([
    init_map_defaults/0,
    percent_div/0,
    init_merger/1,
    get_possible_positions/4,
    get_a_look/6
]).

-spec init_map_defaults() -> list().
init_map_defaults() ->
    [
        {<<"plant">>, 10},
        {<<"rock">>,  50},
        {<<"water">>, 50}
    ].

-spec percent_div() -> integer().
percent_div() -> 1000.

-spec init_merger([{binary(), integer()}]) -> [{binary(), integer()}].
init_merger(L) ->
    orddict:to_list(orddict:merge(
        fun(_,X,_) -> X end, 
        orddict:from_list(L), orddict:from_list(init_map_defaults()))).

-spec get_possible_positions(integer(), integer(), integer(), integer()) -> [integer()].
get_possible_positions(R,C,W,H) ->
    [(R+Dr)*W+C+Dc || Dr<-gpp(R,H),Dc<-gpp(C,W), {Dr, Dc} =/= {R, C}].

gpp(0, _)                -> [0, 1];
gpp(R, H) when R=:=H-1   -> [0, -1];
gpp(R, H) when R>0,R<H-1 -> [-1, 0, 1].

get_a_look(R,C,W,H,Radius,Fun) ->
    lists:flatten([Fun((R+Dr)*W+C+Dc) || Dr<-gpp(R,H,Radius),Dc<-gpp(C,W,Radius), {Dr, Dc} =/= {R, C}]).
    
gpp(0, _, Rad)                      -> lists:seq(0, Rad);
gpp(R, H, Rad) when R=:=H-1         -> lists:seq(-1*Rad, 0);
gpp(R, H, Rad) when R>Rad,R<H-Rad-1 -> lists:seq(-1*Rad, Rad);
gpp(R, _, Rad) when R=<Rad          -> lists:seq(-1*R, Rad);
gpp(R, H, Rad) when R>=H-Rad-1      -> lists:seq(-1*Rad, H-1-R).
