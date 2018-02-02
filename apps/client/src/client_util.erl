-module(client_util).
-include("include/codes.hrl").
-export([encode/1,decode/1,log/1]).

encode({?JOIN_CHAT,<<Alias/binary>>}) ->
    Size = byte_size(Alias),
    <<?JOIN_CHAT,Size,Alias:Size/binary>>;

encode(?LEAVE_CHAT) ->
    <<?LEAVE_CHAT>>;

encode({?JOIN_ROOM,<<Topic/binary>>}) -> 
    Size = byte_size(Topic),
    <<?JOIN_ROOM,Size,Topic:Size/binary>>;

encode({?LEAVE_ROOM,<<Topic/binary>>}) ->
    Size = byte_size(Topic),
    <<?LEAVE_ROOM,Size,Topic:Size/binary>>;

encode({?SEND_CHAT,<<Payload/binary>>}) ->
    Size = byte_size(Payload),
    <<?SEND_CHAT,Size:32,Payload:Size/binary>>;

encode({?SEND_ROOM,<<Topic/binary>>,<<Payload/binary>>}) ->
    TSize = byte_size(Topic),
    PSize = byte_size(Payload),
    <<?SEND_ROOM,TSize,Topic:TSize/binary,PSize:32,Payload:PSize/binary>>;

encode({?SEND_MEMBER,<<To/binary>>,<<Payload/binary>>}) ->
    TSize = byte_size(To),
    PSize = byte_size(Payload),
    <<?SEND_MEMBER,TSize,To:TSize/binary,PSize:32,Payload:PSize/binary>>;

encode(Query) ->
    <<Query>>.

decode(Acc,<<Size,Data:Size/binary,Rest/binary>>) ->
    decode([Data|Acc],Rest);

decode(Acc,<<>>) ->
    Acc.

decode(<<?JOIN_CHAT,Size,Alias:Size/binary>>) -> 
    {?JOIN_CHAT,Alias};

decode(<<?LEAVE_CHAT,Size,Alias:Size/binary>>) ->
    {?LEAVE_CHAT,Alias};

decode(<<?JOIN_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary>>) ->
    {?JOIN_ROOM,Alias,Topic};

decode(<<?LEAVE_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary>>) -> 
    {?LEAVE_ROOM,Alias,Topic};

decode(<<?SEND_CHAT,ASize,Alias:ASize/binary,PSize:32,Payload:PSize/binary>>) ->
    {?SEND_CHAT,Alias,Payload};

decode(<<?SEND_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary,PSize:32,Payload:PSize/binary>>) ->
    {?SEND_ROOM,Alias,Topic,Payload};

decode(<<?SEND_MEMBER,ASize,Alias:ASize/binary,TSize,To:TSize/binary,PSize:32,Payload:PSize/binary>>) -> 
    {?SEND_MEMBER,Alias,To,Payload};

decode(<<?MEMBERS,Rest/binary>>) ->
    Aliases = decode([],Rest),
    {?MEMBERS,Aliases};

decode(<<?ROOMS,Rest/binary>>) ->
    Rooms = decode([],Rest),
    {?ROOMS,Rooms};

decode(<<?MY_ROOMS,Rest/binary>>) ->
    MyRooms = decode([],Rest),
    {?MY_ROOMS,MyRooms};

decode(<<?ALIAS_TAKEN,Size,Alias:Size/binary>>) ->
    {?ALIAS_TAKEN,Alias};

decode(<<?ALIAS_NOT_FOUND,Size,Alias:Size/binary>>) ->
    {?ALIAS_NOT_FOUND,Alias};

decode(<<?UNEXPECTED_CONNECTION>>) ->
    ?UNEXPECTED_CONNECTION;

decode(<<?ROOM_NOT_FOUND,Size,Topic:Size/binary>>) ->
    {?ROOM_NOT_FOUND,Topic};

decode(<<?ALREADY_IN_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary>>) ->
    {?ALREADY_IN_ROOM,Alias,Topic};
  
decode(<<?NOT_IN_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary>>) ->
    {?NOT_IN_ROOM,Alias,Topic};

decode(<<?DECODE_ERROR,Size:32,Msg:Size/binary>>) ->
    {?DECODE_ERROR,Msg}.

log({?JOIN_CHAT,<<Alias/binary>>}) ->
    io:format("~s has joined chat~n",[Alias]);

log({?LEAVE_CHAT,<<Alias/binary>>}) ->
    io:format("~s has left chat~n",[Alias]);

log({?JOIN_ROOM,<<Alias/binary>>,<<Topic/binary>>}) ->
    io:format("~s has joined ~s room~n",[Alias,Topic]);

log({?LEAVE_ROOM,<<Alias/binary>>,<<Topic/binary>>}) ->
    io:format("~s has left ~s room~n",[Alias,Topic]);

log({?SEND_CHAT,<<Alias/binary>>,<<Payload/binary>>}) ->
    io:format("chat> ~s: ~s~n",[Alias,Payload]);

log({?SEND_ROOM,<<Alias/binary>>,<<Topic/binary>>,<<Payload/binary>>}) ->
    io:format("room ~s> ~s: ~s~n",[Topic,Alias,Payload]);

log({?SEND_MEMBER,<<Alias/binary>>,<<_To/binary>>,<<Payload/binary>>}) ->
    io:format("member ~s> ~s~n",[Alias,Payload]);

log({?MEMBERS,Aliases}) ->
    io:format("members: ~p~n",[Aliases]);

log({?ROOMS,Rooms}) ->
    io:format("rooms: ~p~n",[Rooms]);

log({?MY_ROOMS,MyRooms}) ->
    io:format("my_rooms: ~p~n",[MyRooms]);

log({?ALIAS_TAKEN,<<Alias/binary>>}) ->
    error_logger:format("chat already has member with alias: ~s~n",[Alias]);

log({?ALIAS_NOT_FOUND,<<Alias/binary>>}) ->
    error_logger:format("could not find member with alias: ~s~n",[Alias]);

log(?UNEXPECTED_CONNECTION) ->
    error_logger:format("unexpected connection~n",[]);

log({?ROOM_NOT_FOUND,<<Topic/binary>>}) ->
    error_logger:format("~s room not found~n",[Topic]);

log({?ALREADY_IN_ROOM,<<Alias/binary>>,<<Topic/binary>>}) ->
    error_logger:format("~s is already in ~s room~n",[Alias,Topic]);

log({?NOT_IN_ROOM,<<Alias/binary>>,<<Topic/binary>>}) ->
    error_logger:format("~s is not in ~s room ~n",[Alias,Topic]);

log({?DECODE_ERROR,<<Msg/binary>>}) ->
    error_logger:format("could not decode message: ~p~n",[Msg]).