-module(bitcoin).
-compile(export_all).

% Macros
-define(LOG(Value, Base), math:log(Value) / math:log(Base)).
-define(DIGITS_IN(Value, Base), erlang:round(1 + math:floor(?LOG(Value, Base)))).
-define(BINARY_NUMBER_BASE, 2).
-define(HEX_NUMBER_BASE, 16).
% 'big' or 'little'
-define(ENDIANNESS, 'big').
-define(FORMAT_STRING_FOR_BASE(Base, ResultLength, PaddingCharacter),
    "~" ++
        (erlang:integer_to_list(ResultLength)) ++ "." ++
        (erlang:integer_to_list(Base)) ++ "." ++
        PaddingCharacter ++ "x~n"
).
-define(STRING_VALUE_FOR_BASE(Base, DecimalValue, ResultLength, Prefix),
    lists:flatten(
        io_lib:format(?FORMAT_STRING_FOR_BASE(Base, ResultLength, "0"), [DecimalValue, Prefix])
    )
).
-define(BITLENGTH_OF_DIGIT_OF_BASE(Base), ?DIGITS_IN(Base - 1, 2)).
-define(RESULT_LENGTH_OF(Binary, Base),
    erlang:round(math:ceil(erlang:bit_size(Binary) / ?BITLENGTH_OF_DIGIT_OF_BASE(Base)))
).
-define(HASH_ALGORITHM, 'sha256').
-define(ASCII_CHARACTER_LOWER_LIMIT, erlang:round(1 + math:pow(2, 5))).
-define(ASCII_CHARACTER_UPPER_LIMIT, erlang:round(-2 + math:pow(2, 7))).
-define(GATOR_ID, "rushil.patel").
-define(RANDOM_CHARACTER_SEQUENCE_LENGTH, 16).
-define(TAB_CHARACTER_ASCII_CODE, 9).

% Debugging
-ifdef(DEBUG).
-define(LOGGER(X, String),
    io:format(
        "[~p--~p(~p):~p]{~s} ~p~n",
        [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, String, X]
    )
).
-else.
-define(LOGGER(X, String), true).
-endif.

binary_to_decimal(BinaryValue) ->
    binary:decode_unsigned(BinaryValue, ?ENDIANNESS).

calculate_hash_digest(Data) ->
    crypto:hash(?HASH_ALGORITHM, Data).

count_leading_zeros(BinaryValue) ->
    LengthOfBinaryValue = erlang:bit_size(BinaryValue),
    IntegerValue = ?MODULE:binary_to_decimal(BinaryValue),
    FirstSetBitPosition = ?DIGITS_IN(IntegerValue, 2),
    NumberOfLeadingZeros = LengthOfBinaryValue - FirstSetBitPosition,
    erlang:round(NumberOfLeadingZeros).

generate_random_integer_between(LowerLimit, UpperLimit) ->
    N = UpperLimit - LowerLimit + 1,
    LowerLimit + rand:uniform(N) - 1.

generate_random_character_sequence(SequenceLength) when
    SequenceLength > 0
->
    [RandomString] = generate_random_character_sequence(SequenceLength, 1, []),
    RandomString.
generate_random_character_sequence(_, 0, Accumulator) ->
    Accumulator;
generate_random_character_sequence(SequenceLength, NoOfTimes, Accumulator) when
    SequenceLength > 0, NoOfTimes > 0
->
    RandomString = [
        generate_random_integer_between(?ASCII_CHARACTER_LOWER_LIMIT, ?ASCII_CHARACTER_UPPER_LIMIT)
    || _ <- lists:seq(1, SequenceLength)
    ],
    generate_random_character_sequence(
        SequenceLength,
        NoOfTimes - 1,
        Accumulator ++ [RandomString]
    ).

generate_input_to_hash() ->
    ?GATOR_ID ++ generate_random_character_sequence(?RANDOM_CHARACTER_SEQUENCE_LENGTH).

mine_coins(Pid, [], Threshold, Server_Node) ->
    {connect, Server_Node} ! {Pid, get_input};

mine_coins(Pid, [First|Rest] , Threshold, Server_Node) ->
    ?LOGGER(Input, "Input"),
    HashDigest = calculate_hash_digest(First),
    ?LOGGER(HashDigest, "HashDigest"),

    NumberOfLeadingZerosInHashDigest =
        count_leading_zeros(HashDigest) div ?BITLENGTH_OF_DIGIT_OF_BASE(?HEX_NUMBER_BASE),
    ?LOGGER(NumberOfLeadingZerosInHashDigest, "LeadingZeros"),

    DecimalValueOfHashDigest = ?MODULE:binary_to_decimal(HashDigest),

    HashDigestStringInHex = ?STRING_VALUE_FOR_BASE(
        ?HEX_NUMBER_BASE,
        DecimalValueOfHashDigest,
        ?RESULT_LENGTH_OF(HashDigest, ?HEX_NUMBER_BASE),
        ""
    ),
    ?LOGGER(HashDigestStringInHex, "HashDigestStringInHex"),

    case NumberOfLeadingZerosInHashDigest >= Threshold of
        true ->
            io:format("~s~n", [First]),
            {connect, Server_Node} ! {Pid, print, First, HashDigestStringInHex};
        false ->
            mine_coins(Pid, Rest, Threshold, Server_Node)
    end.

connect(Server_Node, Name) ->
    case whereis(client_process) of
        undefined ->
            register(client_process, spawn(?MODULE, client, [Server_Node, Name]));
        _ -> already_connected
    end.

exit() ->
    client_process ! exit.

client(Server_Node, Name) ->
    {connect, Server_Node} ! {self(), join, Name},
    await_input(Server_Node),
    client(Server_Node).

client(Server_Node) ->
    receive
        exit ->
            {connect, Server_Node} ! {self(), exit},
            exit(normal);
        {connect, {Input, Threshold}} ->
            mine_coins(self(), Input, Threshold, Server_Node),
            client(Server_Node)
    end.

%%% wait for a response from the server
await_input(Server_Node) ->
    receive
        {connect, stop, Why} -> % Stop the client 
            io:format("~p~n", [Why]),
            exit(normal);
        {connect, What} ->  % Normal response
            io:format("~p~n", [What]),
            {connect, Server_Node} ! {self(), get_input}
    end.