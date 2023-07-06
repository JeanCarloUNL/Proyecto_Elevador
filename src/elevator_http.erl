-module(elevator_http).
-export([start/0, stop/0, init/2, handle/2, terminate/3]).
-import(elevator, [request/1, move/1, get_floor/0, get_status/0]).

start() ->
    case cowboy:start_clear(elevator_http, [{port, 8080}], #{env => #{dispatch => dispatch_rules()}}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

stop() ->
    cowboy:stop_listener(elevator_http).

init(_Transport, Req) ->
    {ok, Req, #{}}.

handle(Req, State) ->
    io:format("Handle called with req ~p and state ~p~n", [Req, State]),
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path(Req),
    case {Method, Path} of
        {'GET', "/"} ->
            {ok, Req2} = cowboy_req:reply(200, [], <<"Elevator is ready.">>, Req),
            {ok, Req2, State};
        {'POST', "/request"} ->  %% Modificado a POST
            {ok, #{<<"floor">> := Floor}, Req2} = get_body_as_json(Req),
            request(Floor),
            {ok, Req3} = cowboy_req:reply(200, [], <<"Request received.">>, Req2),
            {ok, Req3, State};
        {'POST', "/move"} ->  %% Modificado a POST
            {ok, #{<<"direction">> := Direction}, Req2} = get_body_as_json(Req),
            move(Direction),
            {ok, Req3} = cowboy_req:reply(200, [], <<"Moving.">>, Req2),
            {ok, Req3, State};
        {'GET', "/floor"} ->
            Floor = get_floor(),
            case Floor of
                undefined ->
                    {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
                    {ok, Req2, State};
                _ ->
                    {ok, Req2} = cowboy_req:reply(200, [], list_to_binary(integer_to_list(Floor)), Req),
                    {ok, Req2, State}
            end;
        {'GET', "/status"} ->
            Status = get_status(),
            case Status of
                undefined ->
                    {ok, Req2} = cowboy_req:reply(204, [], <<>>, Req),
                    {ok, Req2, State};
                _ ->
                    {ok, Req2} = cowboy_req:reply(200, [], list_to_binary(atom_to_list(Status)), Req),
                    {ok, Req2, State}
            end;
        _ ->
            {ok, Req2} = cowboy_req:reply(404, [], <<"Not found.">>, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

get_body_as_json(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
   {jsx:decode(Body, [return_maps]), Req2}.

dispatch_rules() ->
    cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, elevator, "index.html"}},
            {"/request", elevator_http, []},
            {"/move", elevator_http, []},
            {"/floor", elevator_http, []},
            {"/status", elevator_http, []}
        ]}
    ]).

