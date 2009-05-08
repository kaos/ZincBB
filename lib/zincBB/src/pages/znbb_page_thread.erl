-module(znbb_page_thread).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Thread View
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

-export([content/1, description/2, main/0, sidebar/1, subject/1, title/0]).

-export([event/1]).

-include("./lib/nitrogen/include/wf.inc").

-include("structs.hrl").

main() ->
    Agent = wf_comet2:comet(fun callback/1),
    Account = znbb_account:retrieve(),
    Tid = wf:get_path_info(),
    case znbb_thread:join(Agent, Account, Tid) of
      {welcome, Thread, Posts} ->
	  #template{file = "thread.html",
		    bindings = [{'Thread', Thread}, {'Posts', Posts}]};
      _Else -> wf_comet2:dismiss(Agent), wf:redirect("/")
    end.

title() -> "Zinc BB".

subject(T) -> ["Viewing: ", T#thread.title].

description(#thread{author = {Name, _Email}}, [LastPost | _]) ->
    Latest = znbb_utils:date(LastPost#post.created),
    [#span{id = userCount, text = " "}, ".", #br{}, "It was created by ", Name,
     ", last updated on ", %todo: Found bug, text = " " is workaround
     #span{id = lastUpdate, text = Latest}, "."].

content(DescPosts) ->
    Posts = lists:reverse(DescPosts),
    LSpan = "span-5 center",
    RSpan = "span-12 last",
    NewPostForm = [#panel{class = LSpan,
			  body =
			      [#h4{class = "prepend-top", text = "Add your comments"},
			       #button{id = postMessageBtn, text = "Reply", postback = new_post}]},
		   #panel{class = RSpan, body = #textarea{id = new_post}}],
    wf:wire(postMessageBtn, new_post,
	    #validate{validators = [#is_required{text = "Cannot be blank"}]}),
    [#panel{id = posts, body = render_posts(Posts)},
     #panel{class = "span-17 zn_newpost append-bottom", body = NewPostForm}].

sidebar(T) ->
    PollArea = case T#thread.poll of
		 none ->
		     wf:wire(pollBtn, #event{type = click, delegate = poll, postback = build_poll}),
		     #button{id = pollBtn, text = "Create a poll"};
		 Poll -> Name = znbb_account:name(), poll:display(Name, Poll)
	       end,
    #panel{body =
	       [#panel{id = poll_area, body = PollArea},
		#h3{text = "Online Users", class = "prepend-top"}, #panel{id = user_area}]}.

render_posts(Posts) -> [render_post(P, old) || P <- Posts].

render_post(#post{author = {Name, Email}, created = C, message = M}, Type) ->
    [#panel{class = "zn_post span-17",
	    body =
		[#panel{class = "span-3 info", body = [avatar(Email), #br{}, Name]},
		 #panel{class = "span-14 last",
			body =
			    [#panel{class = "date fRight",
				    body =
					case Type of
					  old -> znbb_utils:time_diff_now(C);
					  new -> lists:flatten(znbb_utils:hour(C))
					end},
			     #panel{class = "message", body = M}]}]},
     #hr{}].

% ==========================================================
% Events & Callback
% ==========================================================

event(new_post) ->
    Tid = wf:get_path_info(),
    [Post] = wf:q(new_post),
    Author = znbb_account:author(),
    wf:set(new_post, ""),
    znbb_thread:add_post(Author, znbb_utils:sanitize(Post), Tid).

callback({post, P}) ->
    Latest = znbb_utils:date(P#post.created),
    RPost = render_post(P, new),
    wf:update(lastUpdate, Latest),
    wf:insert_bottom(posts, RPost);
callback({user_list, Count, Accounts}) ->
    Users = [avatar(Email) || #account{email = Email} <- Accounts],
    wf:update(userCount, user_count(Count)),
    wf:update(user_area, Users);
callback({Name, {poll, Poll}}) ->
    PollArea = poll:display(Name, Poll), wf:update(poll_area, PollArea).

user_count(1) -> "You're the only person viewing this thread";
user_count(N) ->
    ["There are ", integer_to_list(N), " users reading this thread"].

avatar(guest) -> #image{image = "/static/images/icons/guest.png"};
avatar(Email) -> #gravatar{email = Email, rating = "pg", size = "48"}.
