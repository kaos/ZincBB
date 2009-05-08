-module(poll).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Voting Poll
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

-export([display/2, results/1, results/3]).

-export([event/1]).

-include("./lib/nitrogen/include/wf.inc").

-include("./lib/nitrogen/include/google_chart.inc").

-include("structs.hrl").

display(Name, P) ->
    case gb_sets:is_element(Name, P#poll.voters) of
      true -> results(P);
      false -> vote_form(P)
    end.

results(P) -> results(P, 200, 120).

results(P, Width, Height) ->
    Pairs = lists:sort(dict:to_list(P#poll.ballot)),
    Filtered = [X || {_, Num} = X <- Pairs, Num =/= 0],
    {Labels, Values} = lists:unzip(Filtered),
    Question = P#poll.question,
    wf:wire(chartLink,
	    #event{type = click, delegate = ?MODULE,
		   postback = {big_chart, Question, Labels, Values}}),
    Chart = #link{id = chartLink,
		  body = chart(Width, Height, Question, Labels, Values)},
    [#h3{text = "Poll Results"}, Chart].

vote_form(P) ->
    Choices = lists:sort(dict:fetch_keys(P#poll.ballot)),
    Data = [[C, wf:pickle(C)] || C <- Choices],
    Map = [choice@text, choice@value],
    wf:wire(voteBtn, #event{type = click, delegate = ?MODULE, postback = vote}),
    [#h3{text = P#poll.question, html_encode = true},
     #radiogroup{id = voteGroup,
		 body =
		     [#bind{data = Data, map = Map,
			    body = [#radio{id = choice, html_encode = false}, #br{}]}]},
     #br{}, #button{id = voteBtn, text = "Answer"}].

chart(Width, Height, Question, Labels, Values) ->
    #google_chart{title = Question, type = pie3d, width = Width, height = Height,
		  font_size = 12, axes = [#chart_axis{position = bottom, labels = Labels}],
		  data = [#chart_data{legend = "Data", values = Values}]}.

event(build_poll) ->
    Question = [#label{text = "Question:"}, #br{}, #textbox{id = question}, #br{},
		#br{}],
    Choices = [#label{text = "Answers:"}, #br{}, #textbox{id = option}, #br{},
	       make_option(), make_option(), #panel{id = moreOptions},
	       #link{id = addBtn, text = "Add Choice"}, #br{}, #br{},
	       #button{id = publishBtn, text = "Publish"}],
    PollArea = [#h3{text = "Create your poll"}, [Question, Choices]],
    wf:wire(publishBtn, question,
	    #validate{validators = [#is_required{text = "Required"}]}),
    wf:wire(addBtn,
	    #event{type = click, delegate = ?MODULE, postback = add_option}),
    wf:wire(publishBtn,
	    #event{type = click, delegate = ?MODULE, postback = publish_poll}),
    wf:update(poll_area, PollArea);
event(add_option) -> wf:insert_bottom(moreOptions, make_option());
event(publish_poll) ->
    [Question] = wf:q(question),
    Options = [znbb_utils:sanitize(O) || O <- wf:q(option), O /= []],
    case Options of
      [] -> ok;
      _Else ->
	  Tid = wf:get_path_info(),
	  znbb_thread:add_poll(list_to_binary(Question), Options, Tid)
    end;
event(vote) ->
    case wf:q(voteGroup) of
      [Answer] ->
	  Name = znbb_account:name(),
	  Tid = wf:get_path_info(),
	  znbb_thread:vote(Name, wf:depickle(Answer), Tid);
      _Else -> ignore
    end;
event({big_chart, Question, Labels, Values}) ->
    Chart = wf:render(chart(800, 350, Question, Labels, Values)),
    wf:wire(#script{script = wf_emit:js("modal_box", [Chart])}).

make_option() ->
    Del = wf:temp_id(),
    Id = wf:temp_id(),
    wf:wire(Del, Id,
	    #event{type = click,
		   actions = [#fade{}, #script{script = "$(obj('me')).empty()"}]}),
    #panel{id = Id,
	   body = [#textbox{id = option}, " ", #link{id = Del, text = "X"}]}.
