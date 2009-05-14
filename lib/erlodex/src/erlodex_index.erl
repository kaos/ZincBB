-module(erlodex_index).

-export([search/2, create/1]).

-compile(export_all).


-define(SYMBOL, 0).
-define(PARTIAL, 1).

-define(ENTRY_SZ, 5). % entry size in bytes

-define(UINT, unsigned-native-integer).
-define(BIT, 1/integer).
-define(BYTE, 8/unsigned-integer).
-define(WORD, 16/unsigned-native-integer).
-define(DWORD, 32/unsigned-native-integer).



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

create([]) ->
    {0, {<<>>, <<>>}};
create(Terms) ->
    {SymTbl, Dict} = create(Terms, [], {<<>>, <<>>}),
    Max = byte_size(Dict) - ?ENTRY_SZ,
    {Max, {SymTbl, Dict}}.

create([], _, Index) -> 
    Index;
create([{Term, _Postings} | Rest], Base, Index) ->
    {Type, Symbol, Base2} = frontcompress(Term, Base),
    Index2 = insertentry(Type, Symbol, Index),
    create(Rest, Base2, Index2).

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


% =====================================================================
% Postings Compression
% =====================================================================

compress_postings(Postings) ->
    Gaps = gap_sequence(Postings),
    vbencode_stream(Gaps).

decompress_postings(Postings) ->
    Gaps = vbdecode_stream(Postings),
    ungap_sequence(Gaps).
    
gap_sequence([H|Tail]) ->
    [H | gap_sequence(Tail, H)].
gap_sequence([], _) -> [];
gap_sequence([H|Tail], Last) ->
    [H - Last | gap_sequence(Tail, H)].


ungap_sequence([H|Tail]) ->
    [H | ungap_sequence(Tail, H)].
ungap_sequence([], _) -> [];
ungap_sequence([H|Tail], Last) ->
    Num = H + Last,
    [Num | ungap_sequence(Tail, Num)].



vbencode_stream(ByteStream) ->
    list_to_binary([vbencode(B) || B <- ByteStream]).

vbencode(N) when N < 128 ->
    [(N rem 128) + 128];
vbencode(N) ->
    Bytes = [(N rem 128) + 128],
    N2 = N div 128,
    vbencode1(N2, Bytes).

vbencode1(N, Bytes) when N < 128 ->
    [N rem 128 | Bytes];
vbencode1(N, Bytes) ->
    Bytes2 = [N rem 128 | Bytes],
    N2 = N div 128,
    vbencode1(N2, Bytes2).


vbdecode_stream(ByteStream) ->
    vbdecode(ByteStream, 0, []).

vbdecode(<<>>, _N, Numbers) -> 
    lists:reverse(Numbers);
vbdecode(<<Byte/integer, Rest/binary>>, N, Numbers) when Byte < 128 ->
    vbdecode(Rest, N * 128 + Byte, Numbers);
vbdecode(<<Byte/integer, Rest/binary>>, N, Numbers) ->
    Num = N * 128 + (Byte - 128),
    vbdecode(Rest, 0, [Num | Numbers]).





