-module(populate).

%%%
%  Populate database with randomized test data
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
%%%

-export([start/0, start/1]).

-export([creator/1, start_name_server/1]).

start() -> start(50).

start(N) ->
    {ok, Names} = file:consult("./extra/names"),
    case whereis(name_server) of
      undefined ->
	  Pid = spawn(?MODULE, start_name_server, [Names]), register(name_server, Pid);
      _ -> ok
    end,
    Corpus = corpus(),
    SegLen = trunc(length(Corpus) / N),
    spawn_creators(N, Corpus, SegLen).

start_name_server(Names) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    NameDB = array:from_list(Names),
    Size = array:size(NameDB),
    name_server(NameDB, Size).

name_server(NameDB, Size) ->
    receive
      {From, get} ->
	  Name = array:get(random:uniform(Size) - 1, NameDB),
	  From ! {name, Name},
	  name_server(NameDB, Size)
      after 30000 -> ok
    end.

spawn_creators(0, _, _) -> ok;
spawn_creators(N, Corpus, SegLen) ->
    {Segment, Rest} = take(SegLen, Corpus, []),
    spawn(?MODULE, creator, [Segment]),
    spawn_creators(N - 1, Rest, SegLen).

take(0, Rest, Acc) -> {Acc, Rest};
take(_, [], Acc) -> {Acc, []};
take(N, [H | Rest], Acc) -> take(N - 1, Rest, [H | Acc]).

creator(TextList) ->
    Corpus = array:from_list(TextList),
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    Title = new_title(Corpus),
    {Author, Message} = new_post(Corpus),
    Tid = znbb_vault:create_thread(Title, Author, Message),
    add_replies(random:uniform(50) + 10, Tid, Corpus).

add_replies(0, _Tid, _Corpus) -> ok;
add_replies(N, Tid, Corpus) ->
    {Author, Message} = new_post(Corpus),
    znbb_thread:add_post(Author, Message, Tid),
    add_replies(N - 1, Tid, Corpus).

new_title(Corpus) ->
    Len = array:size(Corpus),
    case array:get(random:uniform(Len) - 1, Corpus) of
      Bin when size(Bin) < 80 -> Bin;
      <<Title:80/integer, _Rest>> -> Title
    end.

new_post(Corpus) ->
    name_server ! {self(), get},
    receive {name, Person} -> Person after 5000 -> Person = <<"Bot">> end,
    Message = build_message(random:uniform(15), Corpus, <<>>),
    {{Person, guest}, Message}.

build_message(0, _Corpus, Post) -> Post;
build_message(C, Corpus, Post) ->
    Len = array:size(Corpus),
    Line = array:get(random:uniform(Len) - 1, Corpus),
    build_message(C - 1, Corpus, <<Post/binary, Line/binary, "\n">>).

corpus() ->
    Corpus = filelib:fold_files("./extra/corpus/", ".+.txt", false,
				fun add_to_corpus/2, []),
    lists:append(Corpus).

add_to_corpus(F, Corpus) ->
    {ok, File} = file:read_file(F), Lines = scanner:lines(File), [Lines | Corpus].
