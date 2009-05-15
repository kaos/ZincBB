-module(erlodex_test).

-compile(export_all).

testsearch(Key) ->
    Pairs = lists:usort(test_terms()),
    IndexDB = erlodex_index:create(Pairs),
    erlodex_index:search(Key, IndexDB).

test_terms() ->
    [{"cable", 25}, {"cat", 30}, {"car", 40}, {"cary", 1}, {"anchor", 1},
     {"anchovie", 5}, {"killer", 19}, {"carton", 20}, {"abracadabra", 31},
     {"animate", 41}, {"killings", 42}, {"killers", 43}, {"auto", 50},
     {"automate", 60}].
