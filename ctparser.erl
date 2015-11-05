-module(ctparser).
-compile(export_all).

-define(U32, 32/unsigned-little-integer).
-define(U16, 16/unsigned-little-integer).
-define(U16B, 16/unsigned-big-integer).

-define(CT_EIGEN, 16#01).
-define(CT_STD,   16#05).
-define(CT_PRINT, 16#06).

-record(eigen, {domain="", serv="", err="", place="", lac="", ci="", drop=""}).

parse([], _) -> ok;
parse([File | Rest], From) ->
    {ok, Bin} = file:read_file(File),
    % FileLen = byte_size(Bin),
    {Records, RecNum} = parseFileHead(Bin),
    io:format("Rec num is ~p~n", [RecNum]),
    parseRecords(Records, RecNum, From),
    parse(Rest, From).


parseFileHead(<<_RncId:2/binary,  RecNum:?U32,
                _Other:74/binary, Records/binary>>)
    -> {Records, RecNum}.

parseRecords(_, 0, _From) -> ok;
parseRecords(<<RecLen:?U16,   Rec:RecLen/binary, 
               _Pad:2/binary, Rest/binary>>, Num, From) ->
    spawn(fun() -> parseRecord(Rec, From) end),
    parseRecords(Rest, Num-1, From).

parseRecord(<<_Len:?U16, _Type:?U16, Tlvlen:?U16, Module:8, 
              _:5/binary, %_vipType:8, _trDataSeq:?U32,
              Valid:8, _vip:3/binary, Gid:?U32, 
              _:4/binary, %_numOcts:8, _pads:3/binary,
              UeId:16/binary, %imsi:8/binary, tmsi:?U32, ptmsi:?U32,
              _GidEx:4/binary, _Tlv:Tlvlen/binary, MsgCount:?U16, _pktSize:?U16,
              Rack:8, Shelf:8, Slot:8, Cpu:8, Msgs/binary>>, From) ->
    RecHead = {Gid, parseUeId(Valid, UeId), Module, Rack, Shelf, Slot, Cpu},
    parseMsgs(Msgs, MsgCount, RecHead);
parseRecord(Rec, _From) -> 
    io:format("Rec size may be wrong with value ~p~n", [Rec]).

parseUeId(16#01, <<IMSI:8/binary, _:8/binary>>) -> formatUeId("IMSI", parseIMSI(IMSI));
parseUeId(16#02, <<_:8/binary, TMSI:4/binary, _:4/binary>>) -> formatUeId("TMSI", binary:bin_to_list(TMSI));
parseUeId(16#08, <<_:12/binary, PTIMS:4/binary>>) -> formatUeId("PTIMS", binary:bin_to_list(PTIMS));
parseUeId(_, _) -> "".

%%Format list to string, such as [1,2,3,4] to "1234"
formatUeId(Type, UeId) -> lists:flatten([Type, ":", lists:map(fun(X)->io_lib:format("~.16B", [X]) end, UeId)]).

%%IMSI format from 214365 to 123456
parseIMSI(IMSI) -> parseIMSI(IMSI, []).
parseIMSI(<<>>, IMSI) -> [_|Rst] = lists:reverse(IMSI), Rst;
parseIMSI(<<H:4, T:4, Rest/binary>>, IMSI) -> parseIMSI(Rest, [H, T | IMSI]).

parseMsgs(_, 0, _) -> ok;
parseMsgs(<<>>, _, _) -> ok;
parseMsgs(<<Seq:?U16, Len:?U16, Second:?U32, 
            MSecond:?U16, Type:8, _:5/binary,
            Msg:Len/binary, _/binary>> = Msgs, Num, RecHead) ->
    {FullMsg, Rest} = split_binary(Msgs, 16 + Len),
    parseMsg(Type, Seq, getTime(Second, MSecond), Msg, FullMsg, RecHead ),
    parseMsgs(Rest, Num-1, RecHead).

%% arg Second is the second from 2000-1-1:00-00-00
%% 63113904000 is the second from 0000-1-1:00-00-00 to 2000-1-1:00-00-00
getTime(Second, MSecond) ->
    {{Year,Mon,Day},{Hour,Min,Sec}} = calendar:gregorian_seconds_to_datetime(63113904000+Second),
    Date = lists:flatten(io_lib:format("~w-~w-~w",[Year,Mon,Day])),
    Time = lists:flatten(io_lib:format("~2.10.0w:~2.10.0w:~2.10.0w:~3.10.0w",[Hour,Min,Sec,MSecond])),
    {Date, Time}.

parseMsg(?CT_EIGEN, Seq, {Date, Time}, <<_:12/binary, Body/binary>>, FullMsg, RecHead) -> 
    % #eigen{domain=Domain, serv=Serv, err=Err, place=Place, lac=Lac, ci=Ci, drop=Drop} = parseEigen(Body);
    io:format("~p Eigen is ~p~n", [self(),parseEigen(Body)]);
parseMsg(?CT_STD, Seq, {_Data, Time}, Msg, FullMsg, RecHead) -> 
    % {Interface, Len, Direct} = parseStd(Msg);
    io:format("~p Std is ~p~n", [self(),parseStd(Msg)]);
parseMsg(?CT_PRINT, Seq, {_Data, Time}, Msg, FullMsg, RecHead) ->
    % {Logid, Pno} = parsePrint(Msg);
    io:format("~p Print is ~p~n", [self(), parsePrint(Msg)]);
parseMsg(Any, _, _, _, _, _) -> io:format("Find unsurpported msg type ~p~n", [Any]).

parseEigen(EigenBody) -> parseEigen(EigenBody, #eigen{}).
parseEigen(_, Eigen) -> Eigen.

parseStd(<<Interface:?U16, _:10/binary, Len:?U16, _:8, Direct:8, _/binary>>) ->{Interface, Len, Direct}.

parsePrint(<<Logid:?U32, Pno:?U32, _/binary>>) -> {Logid, Pno}.
