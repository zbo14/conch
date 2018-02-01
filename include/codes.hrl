%% Message Codes (0-99)
-define(JOIN_CHAT,0).
-define(LEAVE_CHAT,1).
-define(JOIN_ROOM,2).
-define(LEAVE_ROOM,3).
-define(SEND_CHAT,4).
-define(SEND_ROOM,5).
-define(SEND_MEMBER,6).

%% Error Codes (100-199)
-define(ALIAS_TAKEN,100).
-define(ALIAS_NOT_FOUND,101).
-define(UNEXPECTED_CONNECTION,102).
-define(ROOM_NOT_FOUND,103).
-define(ALREADY_IN_ROOM,104).
-define(NOT_IN_ROOM,105).
-define(DECODE_ERROR,106).