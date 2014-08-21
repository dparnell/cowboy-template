-module(template_responder).

-export([respond/4]).

respond(404, _Headers, _Body, Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    error_logger:error_msg("NOT FOUND: ~p~n", [Path]),

    % statman_counter:incr({Path, {error, {404, <<"Not found">>}}}),
    Req2;

respond(Code, _Headers, _Body, Req) when is_integer(Code), Code >= 400 ->
    {Path, Req2} = cowboy_req:path(Req),
    error_logger:error_msg("ERROR ~p: ~p~n", [Code, Path]),
    % statman_counter:incr({Path, {error, {Code, <<"ERROR">>}}}),
    Req2;

respond(_Code, _Headers, _Body, Req) ->
%%    error_logger:info_msg("RESULT: ~p - ~p~n", [Code, Req]),
    Req.
