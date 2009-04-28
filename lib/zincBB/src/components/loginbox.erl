-module(loginbox).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Login box
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
-export([display/0, event/1]).

-include("./lib/nitrogen/include/wf.inc").

-include("structs.hrl").

display() ->
    #panel{id = login_area,
	   body =
	       case wf:role(member) of
		 false -> draw_loginbox();
		 true -> draw_logged_in()
	       end}.

draw_loginbox() ->
    Content = [#textbox{id = userLoginBox,
			actions = "field_default(obj('me'), 'username');", next = passwordLoginBox},
	       #password{id = passwordLoginBox,
			 actions = "field_default(obj('me'), 'username');", next = loginButton},
	       #button{id = loginButton, text = "Login"}],
    wf:wire(loginButton,
	    #event{type = click, delegate = loginbox, postback = login}),
    wf:render(Content).

draw_logged_in() ->
    A = wf:session(account),
    Content = [#panel{class = "",
		      body = ["Welcome: ", #span{text = A#account.user}]},
	       #panel{class = "aRight",
		      body = ["[ ", #link{id = logoutButton, text = "Logout"}, " ]"]}],
    wf:wire(logoutButton,
	    #event{type = click, delegate = loginbox, postback = logout}),
    wf:render(Content).

event(login) ->
    [User] = wf:q(userLoginBox),
    [Pass] = wf:q(passwordLoginBox),
    case znbb_account:authenticate(iolist_to_binary(User), iolist_to_binary(Pass))
	of
      {accepted, _Account} -> wf:redirect(wf_platform:get_raw_path());
      denied -> errorbar:now("Sorry, Incorrect Name/Password"), ok
    end;
event(logout) -> wf:clear_session(), wf_redirect:redirect("/").
