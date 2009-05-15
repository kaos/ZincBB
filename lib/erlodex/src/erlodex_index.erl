-module(erlodex_index).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Erlodex Index Datastructure
%
%%%
%   Copyright 2009 Ceti Forge
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%

-export([search/2, create/1]).

-define(SYMBOL, 0).
-define(PARTIAL, 1).
-define(ENTRY_SZ, 11). % dict entry size in bytes

-define(UINT,    unsigned-native-integer).
-define(BIT,     1/integer).
-define(BYTE,    8/unsigned-integer).
-define(WORD,    16/unsigned-native-integer).
-define(TRIBYTE, 24/unsigned-integer).
-define(DWORD,   32/unsigned-native-integer).

-type index()   :: {binary(), binary(), binary()}. 
-type indexDB() :: {integer(), index()}.
-type termDoc() :: {binary(), integer(), [integer()]}.


% =====================================================================
% Index Search 
% =====================================================================

-spec search(binary(), indexDB()) -> [integer()].
search(Key, {Max, Index}) ->
    case binsearch(Key, Index, 0, Max) of
	not_found -> [];
	{found, Postings} -> Postings
    end.


-spec binsearch(binary(), index(), integer(), integer()) -> not_found | {found, [integer()]}.
binsearch(_Key, _Index, Low, High) when High < Low -> 
    not_found;
binsearch(Key, Index, Low, High) ->
    Mid = Low + (((High - Low) div 2) div ?ENTRY_SZ) * ?ENTRY_SZ,
    Val = fetch(Index, Mid),
    if Val > Key -> binsearch(Key, Index, Low, Mid - ?ENTRY_SZ);
       Val < Key -> binsearch(Key, Index, Mid + ?ENTRY_SZ, High);
       true -> {found, retrieve_postings(Index, Mid)}
    end.


-spec fetch(index(), integer()) -> binary().
fetch({Dict, SymTbl, _PostTbl}=Index, Pos) ->
    <<_:Pos/binary, Type:?BIT, _Freq:31/?UINT, SymPtr:?TRIBYTE, _/binary>> = Dict,
    case Type of
      ?SYMBOL -> 
	  <<_:SymPtr/binary, Len:?BYTE, Symbol:Len/binary, _/binary>> = SymTbl,
	  Symbol;
      ?PARTIAL ->
	  <<_:SymPtr/binary, Len:?BYTE, PreLen:?BYTE, Suffix:Len/binary, _/binary>> = SymTbl,
	  Prefix = fetch_prefix(Index, Pos - ?ENTRY_SZ, PreLen),
	  <<Prefix/binary, Suffix/binary>>
    end.

-spec fetch_prefix(index(), integer(), integer()) -> binary().
fetch_prefix({Dict, SymTbl, _PostTbl}=Index, Pos, PreLen) ->
    <<_:Pos/binary, Type:?BIT, _Freq:31/?UINT, SymPtr:?TRIBYTE, _/binary>> = Dict,
    case Type of
      ?SYMBOL ->
	  <<_:SymPtr/binary, _Len:?BYTE, Prefix:PreLen/binary, _/binary>> = SymTbl,
	  Prefix;
      ?PARTIAL -> fetch_prefix(Index, Pos - ?ENTRY_SZ, PreLen)
    end.


retrieve_postings({Dict, _SymTbl, PostTbl}, Pos) ->
    <<_:Pos/binary, _Type:?BIT, _Freq:31/?UINT, _SymPtr:?TRIBYTE, PostPtr:?DWORD, _/binary>> = Dict,
    <<_:PostPtr/binary, PostLen:?WORD, CompPostings:PostLen/binary, _/binary>> = PostTbl,
    decompress_postings(CompPostings).


% =====================================================================
% Index Creation
% =====================================================================

-spec create([termDoc()]) -> indexDB().
create([]) ->
    {0, {<<>>, <<>>, <<>>}};
create(TermDocs) ->
    {Dict, _SymTbl, _PostTbl} = Index = create(TermDocs, <<>>, {<<>>, <<>>, <<>>}),
    Max = byte_size(Dict) - ?ENTRY_SZ,
    {Max, Index}.


-spec create([termDoc()], binary(), index()) -> index().
create([], _, Index) -> 
    Index;
create([{Term, Freq, Postings} | Rest], Base, {Dict, SymTbl, PostTbl}) ->
    {Type, Symbol, Base2} = frontcompress(Term, Base),
    CompPosts = compress_postings(Postings),

    SymPtr = byte_size(SymTbl),
    PostPtr = byte_size(PostTbl),
    PostLen = byte_size(CompPosts),
    
    Dict2 = <<Dict/binary, Type:?BIT, Freq:31/?UINT, SymPtr:?TRIBYTE, PostPtr:?DWORD>>,
    SymTbl2 = <<SymTbl/binary, Symbol/binary>>,
    PostTbl2 = <<PostTbl/binary, PostLen:?WORD, CompPosts/binary>>,
    create(Rest, Base2, {Dict2, SymTbl2, PostTbl2}).


-spec frontcompress(binary(), binary()) -> {integer(), binary(), binary()}.
frontcompress(Term, Base) ->
    case prefixlen(Term, Base, 0) of
      PreLen when PreLen >= 2 ->
	  <<_Skip:PreLen/binary, Suffix/binary>> = Term,
	  SLen = byte_size(Suffix),
	  {?PARTIAL, <<SLen:?BYTE, PreLen:?BYTE, Suffix/binary>>, Base};
      _ ->
	  Len = byte_size(Term),
	  {?SYMBOL, <<Len:?BYTE, Term/binary>>, Term}
    end.


-spec prefixlen(binary(), binary(), integer()) -> integer().
prefixlen(<<X/integer, Rest1/binary>>, <<X/integer, Rest2/binary>>, Len) ->
    prefixlen(Rest1, Rest2, Len + 1);
prefixlen(_, _, Len) -> 
    Len.

% =====================================================================
% Postings Compression
% =====================================================================

-spec compress_postings([integer()]) -> binary().
compress_postings(Postings) ->
    Gaps = gap_sequence(Postings),
    vbencode_stream(Gaps).

-spec decompress_postings(binary()) -> [integer()].
decompress_postings(Postings) ->
    Gaps = vbdecode_stream(Postings),
    ungap_sequence(Gaps).

-spec gap_sequence([integer()]) -> [integer()].
gap_sequence([H|Tail]) ->
    [H | gap_sequence(Tail, H)].
gap_sequence([], _) -> [];
gap_sequence([H|Tail], Last) ->
    [H - Last | gap_sequence(Tail, H)].

-spec ungap_sequence([integer()]) -> [integer()].
ungap_sequence([H|Tail]) ->
    [H | ungap_sequence(Tail, H)].
ungap_sequence([], _) -> [];
ungap_sequence([H|Tail], Last) ->
    Num = H + Last,
    [Num | ungap_sequence(Tail, Num)].

-spec vbencode_stream([integer()]) -> binary().
vbencode_stream(ByteStream) ->
    list_to_binary([vbencode(B) || B <- ByteStream]).

-spec vbencode(integer()) -> [byte()].
vbencode(N) when N < 128 ->
    [(N rem 128) + 128];
vbencode(N) ->
    Bytes = [(N rem 128) + 128],
    N2 = N div 128,
    vbencode1(N2, Bytes).


-spec vbencode1(integer(), [byte()]) -> [byte()].
vbencode1(N, Bytes) when N < 128 ->
    [N rem 128 | Bytes];
vbencode1(N, Bytes) ->
    Bytes2 = [N rem 128 | Bytes],
    N2 = N div 128,
    vbencode1(N2, Bytes2).

-spec vbdecode_stream(binary()) -> [integer()].
vbdecode_stream(ByteStream) ->
    vbdecode(ByteStream, 0, []).

-spec vbdecode(binary(), integer(), [integer()]) -> [integer()].
vbdecode(<<>>, _N, Numbers) -> 
    lists:reverse(Numbers);
vbdecode(<<Byte/integer, Rest/binary>>, N, Numbers) when Byte < 128 ->
    vbdecode(Rest, N * 128 + Byte, Numbers);
vbdecode(<<Byte/integer, Rest/binary>>, N, Numbers) ->
    Num = N * 128 + (Byte - 128),
    vbdecode(Rest, 0, [Num | Numbers]).

