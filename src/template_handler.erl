%% @doc Render any templates
-module(template_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _Options) ->
    {Template, Req2} = cowboy_req:binding(template, Req),
%    io:format("bindings = ~p~n", [cowboy_req:bindings(Req)]),

    process(Template, Req2).

process(undefined, Req) ->
    {Path, _} = cowboy_req:path_info(Req),
    {ok, Req, file_for(Path)};

process(Template, Req) ->
    process(Template, Req, binary:matches(Template, [<<".dtl">>], [])).

process(Template, Req, []) ->
    {ok, Req, lists:append(binary:bin_to_list(Template), ".dtl")};

process(Template, Req, _) ->
    {ok, Req, binary:bin_to_list(Template)}.

file_for([]) ->
    "index.dtl";
file_for(Path) ->
    binary:bin_to_list(lists:last(Path)).

handle(Req, State) ->
    handle(Req, State, template_exists(State)).

handle(Req, Template, false) ->
    {TemplateModule, Status} = try list_to_existing_atom(re:replace(Template, "\\.", "_", [{return,list}])) of
                         M -> { M, 200 }
                     catch
                         error:badarg -> { error_404_dtl, 404 }
                     end,

    handle(Req, Template, TemplateModule, false, Status);

handle(Req, Template, Found) ->
    TemplateModule = list_to_existing_atom(re:replace(Template, "\\.", "_", [{return,list}])),
    compile(Template, TemplateModule),

    handle(Req, Template, TemplateModule, Found, 200).

handle(Req, Template, TemplateModule, _Found, Status) ->
    {Bindings, _Req} = cowboy_req:bindings(Req),
    SessionVars = template_session:get(Req),

    Vars = lists:append(SessionVars, Bindings),

    {ok, Body} = TemplateModule:render(Vars),

    {ok, Req2} = cowboy_req:reply(Status, [{<<"content-type">>, <<"text/html">>}], Body, Req),

    {ok, Req2, Template}.

terminate(_Reason, _Req, _State) ->
   ok.

compile(Template, Module) when is_binary(Template) ->
    erlydtl:compile(Template, Module);

compile(Template, Module) ->
    erlydtl:compile(template_filename(Template), Module).

template_exists(Template) ->
    filelib:is_regular(template_filename(Template)).

template_filename(Template) ->
    case code:lib_dir(template) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/templates/" ++ Template;
        Priv ->
            Priv ++ "/templates/" ++ Template
    end.
