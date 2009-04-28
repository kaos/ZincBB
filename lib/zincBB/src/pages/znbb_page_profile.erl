-module(znbb_page_profile).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% User proflie configuration
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

-include("./lib/nitrogen/include/wf.inc").

main() -> #template{file = "dual.html"}.

title() -> "Zinc BB".

content() -> "Nothing here yet".
