-module(znbb_mnesia_db).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% ZincBB Mnesia Database
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

-export([create_tables/0, init/0]).

-export([authenticate_user/2, create_account/1, get_thread_listing/0,
	 new_thread/2, persist_thread/1, read_thread/1, write_post/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("structs.hrl").

% ====================================================================
% API
% ====================================================================

init() -> ok = mnesia:start().

get_thread_listing() ->
    F = fun () ->
		Q1 = qlc:q([{Tid, Title, Author, Created}
			    || #thread{tid = Tid, title = Title, author = Author, created = Created}
				   <- mnesia:table(thread)]),
		Q2 = qlc:keysort(4, Q1, [{order, descending}]),
		qlc:e(Q2)
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

new_thread(Thread, Post) ->
    F = fun () -> mnesia:write(Thread), mnesia:write(Post) end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

read_thread(Tid) ->
    F = fun () ->
		Thread = mnesia:read(thread, Tid),
		Posts = mnesia:index_read(post, Tid, tid),
		{Thread, Posts}
	end,
    case mnesia:transaction(F) of
      {atomic, {[T], Posts}} -> {T, Posts};
      _Else -> nil
    end.

persist_thread(Thread) ->
    F = fun () -> mnesia:write(Thread) end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

write_post(Post) ->
    F = fun () -> mnesia:write(Post) end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

create_account(Account) ->
    F = fun () ->
		case mnesia:read(account, Account#account.user, write) of
		  [] -> mnesia:write(Account);
		  _Else -> user_exists
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

authenticate_user(Username, Password) ->
    F = fun () ->
		mnesia:match_object(account,
				    #account{user = Username, password = Password, _ = '_'}, read)
	end,
    case mnesia:transaction(F) of
      {atomic, [Account]} -> {ok, Account};
      _Else -> denied
    end.

% ====================================================================
% Table Creation
% ====================================================================

create_tables() ->
    Nodes = [node()],
    mnesia:create_table(account,
			[{disc_copies, Nodes}, {type, set},
			 {attributes, record_info(fields, account)}]),
    mnesia:create_table(thread,
			[{disc_copies, Nodes}, {type, set},
			 {attributes, record_info(fields, thread)}]),
    mnesia:create_table(post,
			[{disc_only_copies, Nodes}, {type, set}, {index, [tid]},
			 {attributes, record_info(fields, post)}]).
