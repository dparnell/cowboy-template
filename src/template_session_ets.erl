-module(template_session_ets).

-export([init/1, new/3, set/4, get/2, get/3, delete/2, expire/1, debug_set/4]).

-record(state, {tid, conf = [], access}).

-define(SESSION_TIMEOUT, 60*60*24).

%% @private
-spec init([tuple()]) -> {ok, #state{}}.
init(Conf) ->
    Tid = ets:new(template_session_store, [named_table, public, set]),
    AccessId = ets:new(template_session_accesses, [named_table, public, set]),
    {ok, #state{tid = Tid, conf = Conf, access = AccessId}}.

-spec expire(#state{}) -> {ok, #state{}}.
expire(State) ->
%    error_logger:info_msg("Checking for expired sessions~n", []),

    {Mega, Sec, _Micro} = os:timestamp(),
    Now = Mega * 1000000 + Sec,

    ToKill = ets:select(State#state.access, [{{'$1','$2'},[{'<','$2', Now}],['$1']}]),
    lists:foreach(fun(Id) ->
%                          error_logger:info_msg("Expiring session: ~p~n", [Id]),
                          ets:delete(State#state.access, Id),
                          ets:delete(State#state.tid, Id)
                  end, ToKill),

    {ok, State}.

%% @doc Store a new session, unless it's not new.
-spec new(#state{}, any(), cowboy_req:req()) -> {ok, #state{}}.
new(State, SessionId, _Req) ->
    ets:insert_new(State#state.tid, {SessionId, []}),
    {Mega, Sec, _Micro} = os:timestamp(),
    ExpireTime = Mega * 1000000 + Sec + ?SESSION_TIMEOUT,

    ets:insert(State#state.access, {SessionId, ExpireTime}),
    {ok, State}.

%% @doc Set a value.
-spec set(#state{}, any(), any(), cowboy_req:req()) -> {ok, #state{}}.
set(State, Key, Value, Req) ->
    {SessionId, _} = cowboy_req:meta(session_id, Req),
    Session = case ets:lookup(State#state.tid, SessionId) of
                  [{SessionId, Existing}] -> Existing;
                  [] -> []
              end,
    NewSession = lists:keystore(Key, 1, Session, {Key, Value}),
    true = ets:insert(State#state.tid, {SessionId, NewSession}),
    {ok, State}.

%% @doc Set a value.
-spec debug_set(#state{}, any(), any(), any()) -> {ok, #state{}}.
debug_set(State, Key, Value, SessionId) ->
    Session = case ets:lookup(State#state.tid, SessionId) of
                  [{SessionId, Existing}] -> Existing;
                  [] -> []
              end,
    NewSession = lists:keystore(Key, 1, Session, {Key, Value}),
    true = ets:insert(State#state.tid, {SessionId, NewSession}),
    {ok, State}.


%% @doc Get a value.
-spec get(#state{}, any(), cowboy_req:req()) -> {any(), #state{}}.
get(State, Key, Req) ->
    {SessionId, _} = cowboy_req:meta(session_id, Req),
    Value = case ets:lookup(State#state.tid, SessionId) of
                [] -> undefined;
                [{_, Session}] -> proplists:get_value(Key, Session)
            end,
    {Value, State}.

%% @doc Get the whole proplist
-spec get(#state{}, cowboy_req:req()) -> {any(), #state{}}.
get(State, Req) ->
    {SessionId, _} = cowboy_req:meta(session_id, Req),
    Value = case ets:lookup(State#state.tid, SessionId) of
                [] -> undefined;
                [{_, Session}] -> Session
            end,
    {Value, State}.

%% @doc Delete a session.
-spec delete(#state{}, cowboy_req:req()) -> {ok, #state{}}.
delete(State, Req) ->
    {SessionId, _} = cowboy_req:meta(session_id, Req),
    true = ets:delete(State#state.tid, SessionId),
    {ok, State}.
