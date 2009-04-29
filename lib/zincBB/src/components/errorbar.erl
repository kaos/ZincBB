-module(errorbar).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Error bar
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
-include("./lib/nitrogen/include/wf.inc").

-export([messages/0, next/1, now/1]).

messages() ->
    case wf:session(zincbb_flash) of
      undefined -> drawbar("");
      Msg -> wf:session(zincbb_flash, undefined), drawbar(Msg)
    end.

drawbar([]) ->
    #panel{id = error_bar, class = "content", style = "display: none;"};
drawbar(Msg) ->
    #panel{id = error_bar, class = "content", body = create_message(Msg)}.

now(Msg) ->
    wf:update(error_bar, create_message(Msg)), wf:wire(error_bar, #show{}).

next(Msg) -> wf:session(zincbb_flash, Msg).

create_message(Msg) ->
    Hide = #event{type = click, target = error_bar, actions = #hide{}},
    Close = #link{text = "X", actions = Hide},
    Warn = #image{image = "/static/images/warn.png"},
    [Warn, " ", Msg, " ", Close].
