% creature position {CreatureID, CretureType, Position}
-type cr_position()      :: {integer(), binary(), integer(), integer()}.
-type crp_list()         :: [cr_position()].
-type init_p_list()      :: [{binary(), integer()}].
