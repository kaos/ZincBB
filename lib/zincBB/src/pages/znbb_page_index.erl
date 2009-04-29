-module(znbb_page_index).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Index Page
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

-export([content/0, main/0, title/0]).

-export([event/1]).

-include("./lib/nitrogen/include/wf.inc").

main() -> #template{file = "single.html"}.

title() -> "Zinc BB".

content() ->
    Buttons = #button{id = newThreadBtn, text = "New Thread",
		      postback = new_thread},
    case znbb_vault:threads() of
      [] ->
	  Listing = #panel{class = "prepend-top center",
			   body = #h4{text = "No threads have been created yet"}};
      Threads -> Listing = threads(Threads)
    end,
    [#panel{id = new_thread_area, class = "span-24 last zn_newthread",
	    body = Buttons},
     #br{}, #br{}, #hr{}, #panel{class = "span-24 last zn_listing", body = Listing}].

% ==========================================================
% Events
% ==========================================================

event(new_thread) ->
    LSpan = "span-5",
    RSpan = "span-19 last",
    Form = #panel{body =
		      [#h1{text = "Create a new thread"},
		       #panel{class = LSpan,
			      body = #panel{class = "aRight", body = #label{text = "Title"}}},
		       #panel{class = RSpan,
			      body = #textbox{id = newThreadTitle, next = newThreadPost}},
		       #panel{class = LSpan,
			      body =
				  [#panel{class = "aRight", body = #label{text = "First post"}},
				   #p{class = "prepend-top alt", style = "font-size:1.2em",
				      body =
					  "When you have completed your post, click here to create "
					  "your thread. Upon creation, you shall be forwarded to "
					  "your new page."},
				   #panel{class = "center",
					  body =
					      #button{id = createThreadBtn, text = "Create",
						      postback = create_thread}}]},
		       #panel{class = RSpan, body = #textarea{id = newThreadPost}}]},
    wf:wire(createThreadBtn, newThreadTitle,
	    #validate{validators = [#is_required{text = "Please provide a thread title"}]}),
    wf:wire(createThreadBtn, newThreadPost,
	    #validate{validators =
			  [#is_required{text = "You must provide an initial post"}]}),
    wf:update(new_thread_area, Form);
event(create_thread) ->
    [Title] = wf:q(newThreadTitle),
    [Post] = wf:q(newThreadPost),
    Author = znbb_account:author(),
    Tid = znbb_vault:create_thread(znbb_utils:escape(Title), Author,
				   znbb_utils:escape(Post)),
    wf:redirect(["/znbb/thread/", Tid]).

% ==========================================================
% Internal
% ==========================================================

threads(Data) ->
    Map = [thread@class, title@body, title@url, author@body, date@body],
    #table{rows =
	       [#tablerow{cells =
			      [#tableheader{text = "Title", style = "width:auto"},
			       #tableheader{text = "Author", style = "width:150px"},
			       #tableheader{text = "When", style = "width:150px"}]},
		#bind{data = Data, map = Map, transform = fun trans_threads/2,
		      body =
			  #tablerow{id = thread,
				    cells =
					[#tablecell{body = #link{id = title}},
					 #tablecell{id = author, class = "author"},
					 #tablecell{id = date, class = "date"}]}}]}.

trans_threads({Tid, Title, {Name, _Email} = _Author, Created}, even) ->
    Link = <<"/znbb/thread/", Tid/binary>>,
    When = znbb_utils:time_diff_now(Created),
    {["even", Title, Link, Name, When], odd, []};
trans_threads({Tid, Title, {Name, _Email} = _Author, Created}, Acc)
    when Acc == []; Acc == odd ->
    Link = <<"/znbb/thread/", Tid/binary>>,
    When = znbb_utils:time_diff_now(Created),
    {["odd", Title, Link, Name, When], even, []}.
