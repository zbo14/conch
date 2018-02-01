-module(host).
-include("include/codes.hrl").
-behaviour(gen_server).
-record(member,{alias,topics=[],ch,cn}).
-record(state,{dir,lport,members=[],rooms=[],sshd}).
-export([run/0,start/0,start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2,code_change/3]).

run() ->
    ok = ssh:start(),
    application:start(host).

start() ->
    gen_server:start(?MODULE,[],[]).

start_link() ->
    gen_server:start_link(?MODULE,[],[]).

init([]) ->
    process_flag(trap_exit,true),
    Dir = code:priv_dir(host),
    LPort = application:get_env(host,lport,8889),
    {ok,SSHD} = ssh:daemon(LPort,[{system_dir,Dir},{user_dir,Dir},{subsystems,[{"subsystem",{subsystem,[self()]}}]}]),
    State = #state{dir=Dir,lport=LPort,sshd=SSHD},
    {ok,State}.

handle_call(Request,_From,State) ->
    {stop,{unimplemented,call,Request},State}.

handle_cast({CH,CN,Msg},State) ->
    case host_util:decode(Msg) of
        {?DECODE_ERROR,Msg} ->
            respond_error(CH,CN,{?DECODE_ERROR,Msg},State);
        Result -> 
            handle_request(CH,CN,Msg,Result,State)
    end.

handle_request(CH,CN,Msg,{?JOIN_CHAT,Alias},State) ->
    case get_member(Alias,State) of 
        false ->
            Member = #member{alias=Alias,ch=CH,cn=CN},
            new_member(Member,Msg,State);
        _ ->
            respond_error(CH,CN,{?ALIAS_TAKEN,Alias},State)
    end;

handle_request(CH,CN,Msg,Result,State) ->
    case get_member(CN,State) of
        false ->
            respond_error(CH,CN,?UNEXPECTED_CONNECTION,State);
        Member ->
            handle_request(Member,Msg,Result,State)
    end.

handle_request(Member,Msg,?LEAVE_CHAT,State) ->
    remove_from_chat(Member,Msg,State);

handle_request(Member,Msg,{?JOIN_ROOM,Topic},State) ->
    HasRoom = has_room(Topic,State),
    if 
        HasRoom ->
            join_room(Member,Msg,Topic,State);
        true ->
            new_room(Member,Msg,Topic,State)
    end;

handle_request(Member,Msg,{?LEAVE_ROOM,Topic},State) ->
    HasRoom = has_room(Topic,State),
    if
        HasRoom ->
            remove_from_room(Member,Msg,Topic,State);
        true ->
            respond_error(Member,{?ROOM_NOT_FOUND,Topic},State)
    end;

handle_request(Member,Msg,{?SEND_CHAT,_Payload},State) ->
    FullMsg = add_alias(Member,Msg),
    NewState = send_msgs(FullMsg,State),
    {noreply,NewState};

handle_request(Member,Msg,{?SEND_ROOM,Topic,_Payload},State) ->
    HasRoom = has_room(Topic,State),
    if 
        HasRoom -> 
            send_room(Member,Msg,Topic,State);
        true ->
            respond_error(Member,{?ROOM_NOT_FOUND,Topic},State)
    end;

handle_request(Member,Msg,{?SEND_MEMBER,To,_Payload},State) ->
    case get_member(To,State) of 
        false ->
            respond_error(Member,{?ALIAS_NOT_FOUND,To},State);
        Recipient ->
            send_member(Member,Recipient,Msg,State)
    end.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

%% Subroutines

has_room(Topic,#state{rooms=Rooms}) ->
    lists:member(Topic,Rooms).

get_member(<<Alias/binary>>,#state{members=Members}) ->
    lists:keyfind(Alias,#member.alias,Members);

get_member(CN,#state{members=Members}) ->
    lists:keyfind(CN,#member.cn,Members).

remove_member(#member{alias=Alias},#state{members=Members}=State) ->
    NewMembers = lists:keydelete(Alias,#member.alias,Members),
    State#state{members=NewMembers}.

replace_member(Member,State) ->
    #state{members=Members} = remove_member(Member,State),
    State#state{members=[Member|Members]}.

is_room_member(#member{topics=Topics},Topic) ->
    lists:member(Topic,Topics).

add_room(Topic,#state{rooms=Rooms}=State) ->
    State#state{rooms=[Topic|Rooms]}.

add_topic(#member{topics=Topics}=Member,Topic) -> 
    Member#member{topics=[Topic|Topics]}.

remove_topic(#member{topics=Topics}=Member,Topic) ->
    NewTopics = lists:delete(Topic,Topics),
    Member#member{topics=NewTopics}.

same_topics(#member{topics=Topics1},#member{topics=Topics2}) ->
    length(Topics1) =:= length(Topics2).

already_in_room(#member{alias=Alias},Topic) ->
    {?ALREADY_IN_ROOM,Topic,Alias}.

not_in_room(#member{alias=Alias},Topic) ->
    {?NOT_IN_ROOM,Topic,Alias}.

add_alias(#member{alias=Alias},Msg) ->
    add_alias(Alias,Msg);

add_alias(<<Alias/binary>>,<<Type,Rest/binary>>) ->
    Size = byte_size(Alias),
    <<Type,Size,Alias:Size/binary,Rest/binary>>.

join_room(Member,Msg,Topic,State) ->
    IsRoomMember = is_room_member(Member,Topic),
    if
        IsRoomMember ->
            Result = already_in_room(Member,Topic),
            respond_error(Member,Result,State);
        true ->
            add_room_member(Member,Msg,Topic,State)
    end.

new_room(Member,Msg,Topic,State) ->
    NewState = add_room(Topic,State),
    add_room_member(Member,Msg,Topic,NewState).

add_room_member(Member,Msg,Topic,State) ->
    FullMsg = add_alias(Member,Msg),
    NewMember = add_topic(Member,Topic),
    State1 = replace_member(NewMember,State),
    State2 = send_msgs(FullMsg,Topic,State1),
    {noreply,State2}.

new_member(Member,Msg,#state{members=Members}=State) ->
    State1 = State#state{members=[Member|Members]},
    State2 = send_msgs(Msg,State1),
    {noreply,State2}.

remove_from_chat(Member,Msg,State) -> 
    FullMsg = add_alias(Member,Msg),
    State1 = remove_member(Member,State),
    State2 = send_msgs(FullMsg,State1),
    {noreply,State2}.

remove_from_room(Member,Msg,Topic,State) ->
    NewMember = remove_topic(Member,Topic),
    SameTopics = same_topics(Member,NewMember),
    if 
        SameTopics ->
            Result = not_in_room(Member,Topic),
            respond_error(Member,Result,State);
        true ->
            FullMsg = add_alias(Member,Msg),
            State1 = replace_member(NewMember,State),
            State2 = send_msgs(FullMsg,Topic,State1),
            {noreply,State2}
    end.

respond_error(#member{ch=CH,cn=CN},Result,State) -> 
    respond_error(CH,CN,Result,State).

respond_error(CH,CN,Result,State) ->
    Err = host_util:encode(Result),
    ok = ssh_connection:send(CN,CH,Err),
    {noreply,State}.

send_msgs(Msg,#state{members=Members}=State) ->
    NewMembers = lists:filter(
        fun(#member{ch=CH,cn=CN}) ->
            %% TODO: notify members when another member disconnects
            ok =:= ssh_connection:send(CN,CH,Msg)
        end,Members),
    State#state{members=NewMembers}.

send_msgs(Msg,Topic,#state{members=Members}=State) ->
    {RoomMembers,NotRoomMembers} = lists:partition(
        fun(#member{topics=Topics}) ->
            lists:member(Topic,Topics)
        end,Members),
    NewRoomMembers = lists:filter(
        fun(#member{ch=CH,cn=CN}) ->
            %% TODO: notify members when another member disconnects
            ok =:= ssh_connection:send(CN,CH,Msg)
        end,RoomMembers),
    NewMembers = lists:append(NewRoomMembers,NotRoomMembers),
    State#state{members=NewMembers}.

send_room(Member,Msg,Topic,State) -> 
    IsRoomMember = is_room_member(Member,Topic),
    if
        IsRoomMember ->
            FullMsg = add_alias(Member,Msg),
            NewState = send_msgs(FullMsg,Topic,State),
            {noreply,NewState};
        true ->
            Result = not_in_room(Member,Topic),
            respond_error(Member,Result,State)
    end.

send_member(#member{alias=Alias},#member{ch=CH,cn=CN},Msg,State) ->
    FullMsg = add_alias(Alias,Msg),
    ok = ssh_connection:send(CN,CH,FullMsg),
    {noreply,State}.