%% Cowboy Template Application

-module(template).

%% API.
-export([start/0]).

%% API.

start() ->
    error_logger:info_msg("Starting inets~n"),
    ok = application:start(inets),
    error_logger:info_msg("Starting crypto~n"),
    ok = application:start(crypto),
    error_logger:info_msg("Starting asn1~n"),
    ok = application:start(asn1),
    error_logger:info_msg("Starting public_key~n"),
    ok = application:start(public_key),
    error_logger:info_msg("Starting sasl~n"),
    ok = application:start(sasl),
    error_logger:info_msg("Starting ssl~n"),
    ok = application:start(ssl),
    error_logger:info_msg("Starting ranch~n"),
    ok = application:start(ranch),
    error_logger:info_msg("Starting cowlib~n"),
    ok = application:start(cowlib),
    error_logger:info_msg("Starting cowboy~n"),
    ok = application:start(cowboy),
    error_logger:info_msg("Starting erlydtl~n"),
    ok = application:start(erlydtl),
    error_logger:info_msg("Starting template~n"),
    ok = application:start(template).
