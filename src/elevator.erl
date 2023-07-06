-module(elevator).
-export([start/0, stop/0, request/1, move/1, get_floor/0, get_status/0]).
-define(FLOORS, 4).

start() ->
    elevator_http:start(),
    ets:new(elevator_state, [named_table, public]),
    FloorInitResult = ets:insert(elevator_state, {floor, 1}),
    StatusInitResult = ets:insert(elevator_state, {status, 'at bottom'}),
    register(elevator, spawn(fun() -> init(1) end)),
    {FloorInitResult, StatusInitResult}.

stop() ->
    elevator ! stop.

request(Floor) when Floor >= 1, Floor =< ?FLOORS ->
    elevator ! {request, Floor}.

move(Direction) when Direction =:= up; Direction =:= down ->
    elevator ! {move, Direction}.

get_floor() ->
    case ets:lookup(elevator_state, floor) of
        [{floor, Floor}] -> Floor;
        _ -> 'unknown'
    end.

get_status() ->
    case ets:lookup(elevator_state, status) of
        [{status, Status}] -> Status;
        _ -> 'unknown'
    end.

init(Floor) ->
    io:format("Init called with floor ~p~n", [Floor]),
    receive
        stop ->
            ok;
        {request, RequestedFloor} ->
            io:format("Moving to floor ~p~n", [RequestedFloor]),
            FloorChangeResult = ets:insert(elevator_state, {floor, RequestedFloor}),
            Status = if
                RequestedFloor =:= 1 -> 'at bottom';
                RequestedFloor =:= ?FLOORS -> 'at top';
                true -> 'in between'
            end,
            StatusChangeResult = ets:insert(elevator_state, {status, Status}),
            io:format("Floor change result: ~p, status change result: ~p~n", [FloorChangeResult, StatusChangeResult]),
            init(RequestedFloor);
        {move, up} when Floor < ?FLOORS ->
            io:format("Moving up to floor ~p~n", [Floor + 1]),
            FloorChangeResult = ets:insert(elevator_state, {floor, Floor + 1}),
            Status = if
                Floor + 1 =:= ?FLOORS -> 'at top';
                true -> 'in between'
            end,
            StatusChangeResult = ets:insert(elevator_state, {status, Status}),
            io:format("Floor change result: ~p, status change result: ~p~n", [FloorChangeResult, StatusChangeResult]),
            init(Floor + 1);
        {move, down} when Floor > 1 ->
            io:format("Moving down to floor ~p~n", [Floor - 1]),
            FloorChangeResult = ets:insert(elevator_state, {floor, Floor - 1}),
            Status = if
                Floor - 1 =:= 1 -> 'at bottom';
                true -> 'in between'
            end,
            StatusChangeResult = ets:insert(elevator_state, {status, Status}),
            io:format("Floor change result: ~p, status change result: ~p~n", [FloorChangeResult, StatusChangeResult]),
            init(Floor - 1)
    end.