-module(znbb_utils).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
%   Miscellaneous utility functions
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

-export([uuid/0]).

-export([date/1, hour/1, safe_bin/1, time_diff_now/1, timestamp/0]).

-define(MIN, 60).

-define(HOUR, 3600).

-define(DAY, 86400).

-define(WEEK, 604800).

% TODO: Verify this is 'random' enough.
uuid() ->
    <<I:160/integer>> = crypto:sha(term_to_binary({make_ref(), now()})),
    StrId = erlang:integer_to_list(I, 16),
    list_to_binary(StrId).

timestamp() ->
    Now = calendar:universal_time(), calendar:datetime_to_gregorian_seconds(Now).

time_diff_now(Then) ->
    Now = timestamp(),
    case Now - Then of
      Diff when Diff < (?WEEK) -> time_diff(Diff);
      _Else -> date(Then)
    end.

time_diff(Diff) when Diff < 10 -> "A few seconds ago";
time_diff(Diff) when Diff < (?MIN) -> [integer_to_list(Diff), " seconds ago"];
time_diff(Diff) when Diff < (?HOUR) ->
    case trunc(Diff / (?MIN)) of
      1 -> "About one minute ago";
      M -> [integer_to_list(M), " minutes ago"]
    end;
time_diff(Diff) when Diff < (?DAY) ->
    case trunc(Diff / (?HOUR)) of
      1 -> "An hour ago";
      H -> [integer_to_list(H), " hours ago"]
    end;
time_diff(Diff) ->
    case trunc(Diff / (?DAY)) of
      1 -> "Yesterday";
      D -> [integer_to_list(D), " days ago"]
    end.

hour(Secs) ->
    {_, {H, M, S}} = calendar:gregorian_seconds_to_datetime(Secs),
    io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [H, M, S]).

date(Secs) ->
    {{Y, Mo, D}, {H, M, S}} = calendar:gregorian_seconds_to_datetime(Secs),
    [month(Mo),
     io_lib:format(" ~2.10.0B ~4.10.0B, ~2.10.0B:~2.10.0B:~2.10.0B",
		   [D, Y, H, M, S])].

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

safe_bin(Content) when is_list(Content) ->
    wf_utils:js_escape(iolist_to_binary(Content));
safe_bin(Content) -> wf_utils:js_escape(Content).
