-module(host_util).
-include("member.hrl").
-include("include/codes.hrl").
-export([encode/1,decode/1]).

length_prefix(Bin) ->
    Size = byte_size(Bin),
    [Size,Bin].

encode({?MEMBERS,Members}) ->
    Aliases = lists:flatmap(fun(#member{alias=Alias}) -> length_prefix(Alias) end,Members),
    list_to_binary([?MEMBERS|Aliases]);

encode({?ROOMS,Rooms}) ->
    Rooms_ = lists:flatmap(fun(Room) -> length_prefix(Room) end,Rooms),
    list_to_binary([?ROOMS|Rooms_]);

encode({?MY_ROOMS,MyRooms}) ->
    MyRooms_ = lists:flatmap(fun(Room) -> length_prefix(Room) end,MyRooms),
    list_to_binary([?MY_ROOMS|MyRooms_]);

encode({?ALIAS_TAKEN,<<Alias/binary>>}) ->
    Size = byte_size(Alias),
    <<?ALIAS_TAKEN,Size,Alias:Size/binary>>;

encode({?ALIAS_NOT_FOUND,<<Alias/binary>>}) ->
    Size = byte_size(Alias),
    <<?ALIAS_NOT_FOUND,Size,Alias:Size/binary>>;

encode(?UNEXPECTED_CONNECTION) ->
    <<?UNEXPECTED_CONNECTION>>;

encode({?ROOM_NOT_FOUND,<<Topic/binary>>}) ->
    Size = byte_size(Topic),
    <<?ROOM_NOT_FOUND,Size,Topic:Size/binary>>;

encode({?ALREADY_IN_ROOM,<<Alias/binary>>,<<Topic/binary>>}) ->
    ASize = byte_size(Alias),
    TSize = byte_size(Topic),
    <<?ALREADY_IN_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary>>;

encode({?NOT_IN_ROOM,<<Alias/binary>>,<<Topic/binary>>}) ->
    ASize = byte_size(Alias),
    TSize = byte_size(Topic),
    <<?NOT_IN_ROOM,ASize,Alias:ASize/binary,TSize,Topic:TSize/binary>>;

encode({?DECODE_ERROR,<<Msg/binary>>}) ->
    Size = byte_size(Msg),
    <<?DECODE_ERROR,Size:32,Msg:Size/binary>>.

decode(<<?JOIN_CHAT,Size,Alias:Size/binary>>) -> 
    {?JOIN_CHAT,Alias};

decode(<<?LEAVE_CHAT>>) ->
    ?LEAVE_CHAT;

decode(<<?JOIN_ROOM,Size,Topic:Size/binary>>) ->
    {?JOIN_ROOM,Topic};

decode(<<?LEAVE_ROOM,Size,Topic:Size/binary>>) -> 
    {?LEAVE_ROOM,Topic};

decode(<<?SEND_CHAT,Size:32,Payload:Size/binary>>) ->
    {?SEND_CHAT,Payload};

decode(<<?SEND_ROOM,TSize,Topic:TSize/binary,PSize:32,Payload:PSize/binary>>) ->
    {?SEND_ROOM,Topic,Payload};

decode(<<?SEND_MEMBER,TSize,To:TSize/binary,PSize:32,Payload:PSize/binary>>) -> 
    {?SEND_MEMBER,To,Payload};

decode(<<?MEMBERS>>) ->
    ?MEMBERS;

decode(<<?ROOMS>>) ->
    ?ROOMS;

decode(<<?MY_ROOMS>>) ->
    ?MY_ROOMS;

decode(<<Msg/binary>>) ->
    {?DECODE_ERROR,Msg}.