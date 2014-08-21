-module(template_session).
-behaviour(gen_server).

                                                % api
-export([start_link/0, new/1, set/3, get/2, get/1, delete/1, expire/0, debug_set/3, new_id/0]).

                                                % callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {session_store, session_state}).

-define(EXPIRE_INTERVAL, 60000). % One minute

%% API

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
    case application:get_env(template, sessions) of
        undefined -> ignore;
        {ok, Config} -> gen_server:start_link({local, ?MODULE}, ?MODULE, Config, [])
    end.

-spec new_id() -> binary().
new_id() ->
    Data = term_to_binary([make_ref(), now(), random:uniform()]),
    Sha = binary:decode_unsigned(crypto:hash(sha, Data)),
    list_to_binary(lists:flatten(io_lib:format("~40.16.0b", [Sha]))).

%% @doc Creates a new session.
%%
%% Called by {@link template:handle/3} when sessions are configured.
-spec new(cowboy_req:req()) -> cowboy_req:req().
new(Req) ->
    {ExistingId, Req1} = cowboy_req:cookie(<<"session_id">>, Req),
    SessionId = case ExistingId of
                    undefined -> new_id();
                    _ -> ExistingId
                end,
    Req2 = cowboy_req:set_resp_cookie(
             <<"session_id">>, SessionId, cookie_attributes(), Req1),
    Req3 = cowboy_req:set_meta(session_id, SessionId, Req2),
    ok = gen_server:call(?MODULE, {new, [SessionId, Req3]}),
    Req3.


%% @doc Sets a value in the session.
-spec debug_set(any(), any(), any()) -> any().
debug_set(Key, Value, SessionId) ->
    gen_server:call(?MODULE, {debug_set, [Key, Value, SessionId]}).

%% @doc Sets a value in the session.
-spec set(any(), any(), cowboy_req:req()) -> any().
set(Key, Value, Req) ->
    gen_server:call(?MODULE, {set, [Key, Value, Req]}).

%% @doc Gets a value from the session.
-spec get(any(), cowboy_req:req()) -> any().
get(Key, Req) ->
    gen_server:call(?MODULE, {get, [Key, Req]}).

%% @doc Gets a value from the session.
-spec get(cowboy_req:req()) -> any().
get(Req) ->
    gen_server:call(?MODULE, {get, [Req]}).

%% @doc Deletes a session.
-spec delete(cowboy_req:req()) -> any().
delete(Req) ->
    gen_server:call(?MODULE, {delete, [Req]}).

-spec expire() -> ok.
expire() ->
    gen_server:cast(?MODULE, expire).

%% CALLBACKS

%% @private
-spec init([tuple()]) -> {ok, #state{}}.
init(Config) ->
    {store, SessionStore, SessionConf} = case proplists:lookup(store, Config) of
                                             none -> {store, template_session_ets, []};
                                             Else -> Else
                                         end,
    {ok, SessionState} = SessionStore:init(SessionConf),
    erlang:send_after(?EXPIRE_INTERVAL, self(), expire),

    {ok, #state{session_state = SessionState, session_store = SessionStore}}.


%% @private
%% @doc Delegates the call to the configured session store module.
-spec handle_call({atom(), [any()]}, {pid(),_}, #state{}) -> {reply, any(), #state{}}.
handle_call({F, A}, _From, State) ->
    M = State#state.session_store,
    SessionState = State#state.session_state,
    {Ret, NewSessionState} = apply(M, F, [SessionState | A]),
    {reply, Ret, State#state{session_state = NewSessionState}}.

%% @private
handle_cast(expire, State) ->
    M = State#state.session_store,
    SessionState = State#state.session_state,
    M:expire(SessionState),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(expire, State) ->
    ok = expire(),
    erlang:send_after(?EXPIRE_INTERVAL, self(), expire),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% INTERNAL


%% @private
cookie_attributes() ->
    {ok, Config} = application:get_env(template, sessions),
    Attrs = proplists:get_value(cookies, Config, []),
    lists:keystore(path, 1, Attrs, {path, <<"/">>}).
