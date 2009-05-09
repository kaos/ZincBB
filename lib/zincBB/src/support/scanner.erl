-module(scanner).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
%  Simple text scanning and tokenizer
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

-export([lines/1, tokens/1]).

-define(TOKENIZER, "\\W+").

-define(LINES, "\\s*[\\Q.!$\\E\\r\\n]+\\s*").

tokens(Input) ->
    Tokens = re:split(Input, ?TOKENIZER, [{return, binary}, trim]), length(Tokens).

lines(Input) -> re:split(Input, ?LINES, [{return, binary}, trim]).
