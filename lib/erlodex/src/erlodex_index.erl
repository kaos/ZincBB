-module(erlodex_index).

-compile(export_all).


-define(SYMBOL, 0).
-define(PARTIAL, 1).

-define(ENTRY_SZ, 5). % entry size in bytes

-define(UINT, unsigned-native-integer).
-define(BIT, 1/integer).
-define(BYTE, 8/unsigned-integer).
-define(WORD, 16/unsigned-native-integer).
-define(DWORD, 32/unsigned-native-integer).


testsearch(Key) ->
    Pairs = lists:usort(testwords()),
    IndexDB = writeindex(Pairs),
    search(Key, IndexDB).


% =====================================================================
% Index Search 
% =====================================================================

search(Key, {Max, Index}) ->
    case binsearch(Key, Index, 0, Max) of
	not_found -> io:format("Could not find that~n", []);
	found -> io:format("Yeah what a success~n", [])
    end.

binsearch(_Key, _Index, Low, High) when High < Low -> 
    not_found;
binsearch(Key, Index, Low, High) ->
    Mid = Low + (((High - Low) div 2) div ?ENTRY_SZ) * ?ENTRY_SZ,
    Val = fetch(Index, Mid),
    if Val > Key -> binsearch(Key, Index, Low, Mid - ?ENTRY_SZ);
       Val < Key -> binsearch(Key, Index, Mid + ?ENTRY_SZ, High);
       true -> found
    end.

fetch({SymTbl, Dict} = Index, Pos) ->
    <<_:Pos/binary, Type:?BIT, SymPtr:?WORD, _Freq:23/?UINT, _/binary>> = Dict,
    case Type of
      ?SYMBOL -> 
	  <<_:SymPtr/binary, Len:?BYTE, Symbol:Len/binary, _/binary>> = SymTbl,
	  Symbol;
      ?PARTIAL ->
	  <<_:SymPtr/binary, Len:?BYTE, PreLen:?BYTE, Suffix:Len/binary, _/binary>> = SymTbl,
	  Prefix = fetch_prefix(Index, Pos - ?ENTRY_SZ, PreLen),
	  <<Prefix/binary, Suffix/binary>>
    end.

fetch_prefix({SymTbl, Dict} = Index, Pos, PreLen) ->
    <<_:Pos/binary, Type:?BIT, SymPtr:?WORD, _Freq:23/?UINT, _/binary>> = Dict,
    case Type of
      ?SYMBOL ->
	  <<_:SymPtr/binary, _Len:?BYTE, Prefix:PreLen/binary, _/binary>> = SymTbl,
	  Prefix;
      ?PARTIAL -> fetch_prefix(Index, Pos - ?ENTRY_SZ, PreLen)
    end.


% =====================================================================
% Index Creation
% =====================================================================

writeindex(Terms) ->
    {SymTbl, Dict} = writeindex(Terms, [], {<<>>, <<>>}),
    Max = byte_size(Dict) - ?ENTRY_SZ,
    {Max, {SymTbl, Dict}}.

writeindex([], _, Index) -> Index;
writeindex([{Term, _Postings} | Rest], Base, Index) ->
    {Type, Symbol, Base2} = frontcompress(Term, Base),
    Index2 = insertentry(Type, Symbol, Index),
    writeindex(Rest, Base2, Index2).

frontcompress(Term, Base) ->
    case prefixlen(Term, Base, 0) of
      PreLen when PreLen >= 2 ->
	  Symbol = list_to_binary(lists:nthtail(PreLen, Term)),
	  SLen = byte_size(Symbol),
	  {?PARTIAL, <<SLen:?BYTE, PreLen:?BYTE, Symbol/binary>>, Base};
      _ ->
	  Symbol = list_to_binary(Term),
	  SLen = byte_size(Symbol),
	  {?SYMBOL, <<SLen:?BYTE, Symbol/binary>>, Term}
    end.

prefixlen([X | Rest1], [X | Rest2], Len) -> prefixlen(Rest1, Rest2, Len + 1);
prefixlen(_, _, Len) -> Len.

insertentry(Type, Symbol, {SymTbl, Dict}) ->
    SymPtr = byte_size(SymTbl),
    Freq = 1,
    Entry = <<Type:?BIT, SymPtr:?WORD, Freq:23/?UINT>>,
    SymTbl2 = <<SymTbl/binary, Symbol/binary>>,
    Dict2 = <<Dict/binary, Entry/binary>>,
    {SymTbl2, Dict2}.

testwords() ->
    [{"cable", 25}, {"cat", 30}, {"car", 40}, {"cary", 1}, {"anchor", 1},
     {"anchovie", 5}, {"killer", 19}, {"carton", 20}, {"abracadabra", 31},
     {"animate", 41}, {"killings", 42}, {"killers", 43}, {"auto", 50},
     {"automate", 60}].

