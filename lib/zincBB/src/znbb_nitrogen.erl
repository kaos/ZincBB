-module(znbb_nitrogen).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Nitrogen Callbacks
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

-export([loop/2, request/1, route/1]).

%% Loop/2 lets you define the main dispatch loop for the nitrogen
%% framework.

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
      Method when Method =:= 'GET'; Method =:= 'HEAD' ->
	  case Path of
	    "" -> wf_mochiweb:loop(Req, znbb_page_index);
	    "znbb/" ++ _ -> wf_mochiweb:loop(Req);
	    "static/" ++ Static -> Req:serve_file(Static, DocRoot);
	    _ -> Req:not_found()
	  end;
      'POST' ->
	  case Path of
	    "" -> wf_mochiweb:loop(Req, znbb_page_index);
	    "znbb/" ++ _ -> wf_mochiweb:loop(Req);
	    _ -> Req:not_found()
	  end;
      _ -> Req:respond({501, [], []})
    end.

%% route/1 lets you define new URL routes to your web pages,
%% or completely create a new routing scheme.
%% The 'Path' argument specifies the request path. Your
%% function should return either an atom which is the page module
%% to run, or a tuple containing {Module, PathInfo}. PathInfo
%% can be accessed using wf:get_path_info().
%%
%% Uncomment the line below to direct requests
%% from "/web/newroute" to the web_index module:
%%
%% route("/web/newroute") -> web_index;
%%
%% Uncomment the line below to direct requests
%% from "/web/newroute" to the web_index module,
%% with trailing PathInfo included:
%%
%% route("/web/newroute/" ++ PathInfo) -> {web_index, PathInfo};

route(Path) ->
    case string:tokens(Path, "/") of
      ["znbb", Page | Info] ->
	  Mod = try list_to_existing_atom("znbb_page_" ++ Page) catch
		  error:_ -> znbb_page_index
		end,
	  route(Mod, Info);
      _Else -> {znbb_page_index, ""}
    end.

route(znbb_page_thread, [Tid]) ->
    BTid = list_to_binary(Tid), {znbb_page_thread, BTid};
route(Module, Info) -> {Module, Info}.

%% request/1 is executed before every Nitrogen page, and lets
%% you add authentication and authorization. The 'Module' argument
%% is the name of the page module.
%% This function should return either 'ok' if processing can proceed,
%% or it can return a full-fledged page by treating it just like the main function
%% of a page. Alternatively, you can use the wf:redirect* functions to
%% issue a client-side redirect to a new page.

% White Listed pages (not protected)
request(znbb_page_index) -> ok;
request(znbb_page_thread) -> ok;
% Deny these pages to members
request(znbb_page_signup) -> non_member();
% Protect all other pages.
request(_) -> member_only().

member_only() ->
    case wf:role(member) of
      true -> ok;
      false -> errorbar:next("Please Login First"), wf:redirect("/")
    end.

non_member() ->
    case wf:role(member) of
      true -> wf:redirect("/");
      false -> ok
    end.
