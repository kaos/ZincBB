-module(znbb_account).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
%  User account management
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

-export([authenticate/2, author/0, create/3, name/0, retrieve/0]).

-include("settings.hrl").

-include("structs.hrl").

name() ->
    case wf:session(account) of
      undefined -> guest_name();
      A -> A#account.user
    end.

retrieve() ->
    case wf:session(account) of
      undefined -> #account{user = guest_name(), email = guest};
      A -> A
    end.

author() ->
    case wf:session(account) of
      undefined -> {guest_name(), guest};
      A -> {A#account.user, A#account.email}
    end.

guest_name() -> iolist_to_binary(["Guest@", client_ip()]).

% Parts adapted from mochiweb_request.erl
client_ip() ->
    Socket = wf_platform:get_socket(),
    case inet:peername(Socket) of
      {ok, {Addr = {10, _, _, _}, _Port}} ->
	  case wf_platform:get_header('x-forwarded-for') of
	    undefined -> inet_parse:ntoa(Addr);
	    Hosts -> string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      {ok, {{127, 0, 0, 1}, _Port}} ->
	  case wf_platform:get_header('x-forwarded-for') of
	    undefined -> "127.0.0.1";
	    Hosts -> string:strip(lists:last(string:tokens(Hosts, ",")))
	  end;
      {ok, {Addr, _Port}} -> inet_parse:ntoa(Addr)
    end.

create(Username, Password, Email) ->
    MD5Pass = crypto:md5(Password),
    Account = #account{user = Username, password = MD5Pass, email = Email},
    case (?DB_ENGINE):create_account(Account) of
      ok -> setup_account(Account), {ok, Account};
      user_exists -> {error, "Choose another user name, that one is taken!"}
    end.

authenticate(Username, Password) ->
    MD5Pass = crypto:md5(Password),
    case (?DB_ENGINE):authenticate_user(Username, MD5Pass) of
      {ok, Account} -> setup_account(Account), {accepted, Account};
      denied -> denied
    end.

setup_account(Account) -> wf:role(member, true), wf:session(account, Account).
