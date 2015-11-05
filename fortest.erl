-module(fortest).
-compile(export_all).

fib(N) -> fib(N, 0, 1).
fib(0, A, _) -> A;
fib(N, A, B) -> fib(N-1, B, A+B).

-define(U32, 32/unsigned-little-integer).
-define(U16, 16/unsigned-little-integer).

parse(File) ->
    {ok, Bin} = file:read_file(File),
    % FileLen = byte_size(Bin),
    {RecNum, Bin1} = parseFileHead(Bin),
    parseRecords(Bin1, 50).

parseFileHead(<<_RncId:2/binary, RecNum:?U32,
                _Other:74/binary,Rest/binary>>) -> {RecNum, Rest}.


parseRecords(_, 0) -> ok;
parseRecords(<<RecLen:?U16, _Len:?U16, _Type:?U16, Tlvlen:?U16, Module:8, _:5/binary, 
            Valid:8, _vip:3/binary, Gid:?U32, _:4/binary, %_numOcts:8, _pads:3/binary,
            UeId:16/binary, %imsi:8/binary, tmsi:?U32, ptmsi:?U32,
            _GidEx:4/binary, _Tlv:Tlvlen/binary,
            MsgCount:?U16, _pktSize:?U16, Rack:8, Shelf:8, Slot:8, Cpu:8,
            Msgs:_pktSize/binary, _Pad:2/binary, _Rest/binary>>, RecNum) ->
    TLen = parseMsgs(Msgs, MsgCount, 0),
    RecHead = {RecLen, Module, Rack, Shelf, Slot, Cpu, _pktSize, Tlvlen, MsgCount, TLen, TLen+Tlvlen+52},
    io:format("begin to parse msgs with:~p~n", [RecHead]),
    parseRecords(_Rest, RecNum-1).

parseMsgs(_, 0, Len) -> Len;
parseMsgs(<<>>, _, Len) -> Len;
parseMsgs(<<_msgSeq:?U16, MsgLen:?U16, _Second:?U32, 
            _MSecond:?U16, _MsgClass:8, _:5/binary,
            _Msg:MsgLen/binary, OtherMsgs/binary>>, Num, Len) ->
    % io:format("Msg info is ~p ~n", [{_msgSeq, MsgLen, _MsgClass}]),
    parseMsgs(OtherMsgs, Num-1, Len+MsgLen+16).