-module(erlodex_test).

-compile(export_all).

testsearch() ->
    Input = lists:usort(test_terms()),
    IndexDB = erlodex_index:create(Input),
    [erlodex_index:search(Key, IndexDB) =:= Val || {Key,_F,Val} <- Input].


test_terms() -> [
    {<<"cable">>, 1,       [25, 900, 40032]},
    {<<"cat">>, 1,         [30]},
    {<<"car">>, 1,         [40, 41, 900]},
    {<<"carried">>, 1,     [1, 10, 18, 4992]},
    {<<"anchor">>, 1,      [1, 500, 688, 3216]},
    {<<"anchovies">>, 1,   [5, 6]},
    {<<"killer">>, 1,      [4000, 8001]},
    {<<"carton">>, 1,      [20, 22]},
    {<<"abracadabra">>, 1, [7081, 9082]},
    {<<"animate">>, 1,     [21321]},
    {<<"killings">>, 1,    [324]},
    {<<"killers">>, 1,     [234, 2343, 98269]},
    {<<"auto">>, 1,        [32, 49, 100]},
    {<<"automate">>, 1,    [60, 602, 3000, 4032, 5026, 5027, 9021]}
].


