-module(znbb_thread).

-author("Tom McNulty <tom.mcnulty@cetiforge.com>").

%%%
% Thread
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

%% API
-export([add_poll/3, add_post/3, create/1, join/3, shutdown/1, vote/3]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	 terminate/2]).

-include("structs.hrl").

-define(THREAD_LIFE, 600000). % 10 minutes in ms

-record(state, {thread, posts, users = gb_trees:empty()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

% Grabs the latest posts, and joins the user to the thread
join(UserPid, Account, Tid) -> send_call({join, UserPid, Account}, Tid).

add_post(Author, Message, Tid) ->
    SMessage = znbb_utils:sanitize(Message),
    gen_server:cast({global, Tid}, {add_post, Author, SMessage}).

add_poll(Question, Options, Tid) ->
    gen_server:cast({global, Tid}, {add_poll, Question, Options}).

vote(Name, Answer, Tid) -> gen_server:cast({global, Tid}, {vote, Name, Answer}).

create(Tid) -> gen_server:start({global, Tid}, ?MODULE, [Tid], []).

shutdown(Tid) -> gen_server:cast({global, Tid}, stop).

send_call(Cmd, Tid) ->
    try gen_server:call({global, Tid}, Cmd) catch
      exit:{noproc, _} ->
	  case create(Tid) of
	    {ok, Pid} -> gen_server:call(Pid, Cmd);
	    _ -> {error, bad_thread}
	  end
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Tid]) ->
    case znbb_vault:read_thread(Tid) of
      nil -> ignore;
      {Thread, Posts} ->
	  OrderedPosts = lists:reverse(lists:keysort(6, Posts)),
	  State = #state{thread = Thread, posts = OrderedPosts},
	  process_flag(trap_exit, true),
	  {ok, State, ?THREAD_LIFE}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({join, UserPid, Account}, _From, S) ->
    #state{thread = T, posts = P, users = U} = S,
    erlang:link(UserPid),
    U2 = gb_trees:enter(UserPid, Account, U),
    broadcast_userlist(U2),
    Reply = {welcome, T, P},
    {reply, Reply, S#state{users = U2}, ?THREAD_LIFE};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State, ?THREAD_LIFE}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_post, Author, Message}, S) ->
    #state{thread = T, users = US, posts = PS} = S,
    Post = znbb_vault:create_post(Author, Message, T#thread.tid),
    broadcast_message({post, Post}, US),
    S2 = S#state{posts = [Post | PS]},
    {noreply, S2};
handle_cast({add_poll, Question, Options}, S) ->
    #state{thread = T, users = US} = S,
    Ballot = dict:from_list([{O, 0} || O <- Options]),
    Poll = #poll{question = Question, ballot = Ballot},
    broadcast_named_message({poll, Poll}, US),
    T2 = T#thread{poll = Poll},
    znbb_vault:persist_thread(T2),
    {noreply, S#state{thread = T2}, ?THREAD_LIFE};
handle_cast({vote, Name, Answer}, S) ->
    #state{thread = T, users = US} = S,
    P = #poll{voters = Voters, ballot = Ballot} = T#thread.poll,
    case {gb_sets:is_member(Name, Voters), dict:is_key(Answer, Ballot)} of
      {false, true} ->
	  Voters2 = gb_sets:insert(Name, Voters),
	  Ballot2 = dict:update_counter(Answer, 1, Ballot),
	  Poll2 = P#poll{voters = Voters2, ballot = Ballot2},
	  broadcast_named_message({poll, Poll2}, US),
	  T2 = T#thread{poll = Poll2},
	  znbb_vault:persist_thread(T2),
	  {noreply, S#state{thread = T2}, ?THREAD_LIFE};
      _Otherwise -> {noreply, S, ?THREAD_LIFE}
    end;
handle_cast(stop, S) -> {stop, normal, S};
handle_cast(_Msg, State) -> {noreply, State, ?THREAD_LIFE}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) -> {stop, normal, State};
handle_info({'EXIT', UserPid, _Reason}, S) ->
    U2 = gb_trees:delete_any(UserPid, S#state.users),
    S2 = S#state{users = U2},
    case gb_trees:is_empty(U2) of
      true -> {stop, normal, S2};
      false -> broadcast_userlist(U2), {noreply, S#state{users = U2}, ?THREAD_LIFE}
    end;
handle_info(_Info, State) -> {noreply, State, ?THREAD_LIFE}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{users = Users}) ->
    [wf_comet2:dismiss(U) || U <- gb_trees:keys(Users)], ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal Helpers
%%--------------------------------------------------------------------

broadcast_userlist(Users) ->
    Accounts = lists:usort(gb_trees:values(Users)),
    Count = gb_trees:size(Users),
    [U ! {user_list, Count, Accounts} || U <- gb_trees:keys(Users)],
    ok.

broadcast_message(Message, Users) ->
    [U ! Message || U <- gb_trees:keys(Users)], ok.

broadcast_named_message(Message, Users) ->
    [U ! {Name, Message} || {U, #account{user = Name}} <- gb_trees:to_list(Users)],
    ok.
