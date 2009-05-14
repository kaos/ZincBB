-module(znbb_vault).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Manages Threads
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

-export([create_post/3, create_thread/3, persist_thread/1, read_thread/1,
	 threads/0]).

-include("settings.hrl").

-include("structs.hrl").

threads() -> (?DB_ENGINE):get_thread_listing().

create_thread(T, Author, M) ->
    Title = znbb_utils:sanitize(T),
    Message = znbb_utils:sanitize(M),
    Now = znbb_utils:timestamp(),
    Tid = znbb_distid:get(),
    PostId = znbb_distid:get(),
    Post = #post{postid = PostId, tid = Tid, author = Author, message = Message,
		 created = Now},
    Thread = #thread{tid = Tid, title = Title, author = Author, created = Now},
    ok = (?DB_ENGINE):new_thread(Thread, Post),
    znbb_thread:create(Tid),
    Tid.

read_thread(Tid) -> (?DB_ENGINE):read_thread(Tid).

persist_thread(Thread) -> (?DB_ENGINE):persist_thread(Thread).

create_post(Author, Message, Tid) ->
    Now = znbb_utils:timestamp(),
    PostId = znbb_distid:get(),
    Post = #post{postid = PostId, tid = Tid, author = Author, message = Message,
		 created = Now},
    ok = (?DB_ENGINE):write_post(Post),
    Post.
