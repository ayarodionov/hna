-module(newsroom_test).


-include_lib("eunit/include/eunit.hrl").


separate_test() ->
	?assertMatch(
		{[6,4,2],[5,3,1]}, 
		newsroom:separate([1,2,3,4,5,6], [{1,a},{3,b},{5,c}])),
	ok.

separate_new_ones_test() ->
	?assertMatch(
		[6,4,2], 
		newsroom:new_ones([1,2,3,4,5,6], [{1,a},{3,b},{5,c}])),
	ok.

separate_pick_test() ->
	?assertMatch(
		[{5,c},{1,a}], 
		newsroom:pick([5,2,1], [{1,a},{3,b},{5,c}])),
	ok.

find_pos_test() ->
	?assertMatch(
		1, 
		newsroom:find_pos(1, [{1,a},{3,b},{5,c}])),
	?assertMatch(
		2, 
		newsroom:find_pos(3, [{1,a},{3,b},{5,c}])),
	?assertMatch(
		-1, 
		newsroom:find_pos(55, [{1,a},{3,b},{5,c}])),
	ok.
