-module(superb).
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
  [string:concat(?GATOR_ID, RandomString) || RandomString <- generate_random_character_sequence(?RANDOM_CHARACTER_SEQUENCE_LENGTH, 10, [])].
  % ?GATOR_ID ++ generate_random_character_sequence(?RANDOM_CHARACTER_SEQUENCE_LENGTH).

mine_coins([], Threshold, _) ->
 mine_coins(generate_input_to_hash(), Threshold, 1);
 
mine_coins(_,Threshold,0) -> 
	{_,A} = statistics(runtime),
	{_,B} = statistics(wall_clock),
    io:format("~p~n", [A / B]),
    mine_coins(generate_input_to_hash(), Threshold, 1);
    
mine_coins([Input|Rest], Threshold, NoOfCoins) ->
    ?LOGGER(Input, "Input"),

    HashDigest = calculate_hash_digest(Input),
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
            io:format(
                "~s~c~s~n",
                [Input, ?TAB_CHARACTER_ASCII_CODE, HashDigestStringInHex]
            ),
        mine_coins(Rest, Threshold, NoOfCoins - 1);
        false ->
            mine_coins(Rest, Threshold, NoOfCoins)
    end.

start_mining(Threshold, NoOfCoins) ->
    Input = generate_input_to_hash(),
    mine_coins(Input, Threshold, NoOfCoins).

%%% Change the function below to return the name of the node where the
%%% connect server runs
server_node() ->
    server@raj.

server(Threshold, Client_List) ->
    receive
        {Client_ID, join, Name} ->
            New_Client_List = add_client(Client_ID, Name, Client_List),
            io:format("list is now: ~p~n", [New_Client_List]),
            server(Threshold, New_Client_List);
        {Client_ID, exit} ->
            New_Client_List = remove_client(Client_ID, Client_List),
            io:format("list is now: ~p~n", [New_Client_List]),
            server(Threshold, New_Client_List);
        {Client_ID, get_input} -> 
            Client_ID ! {connect, {generate_input_to_hash(), Threshold}},
            server(Threshold, Client_List);
        {Client_ID, print, Input, HashDigestStringInHex} ->
            io:format(
                "~s~c~s~n",
                [Input, ?TAB_CHARACTER_ASCII_CODE, HashDigestStringInHex]
            ),
            Client_ID ! {connect, {generate_input_to_hash(), Threshold}},
            server(Threshold, Client_List)
    end.

start_server(Threshold) ->
    statistics(runtime),
    statistics(wall_clock),
    register(connect, spawn(?MODULE, server, [Threshold, []])),
    start_mining(Threshold, 1).
    % FirstStatistic = statistics(wall_clock),

add_client(Client_ID, Name, Client_List) -> 
    case lists:keymember(Name, 2, Client_List) of
        true ->
            Client_ID ! {connect, stop, client_exists_at_other_node},  %reject logon
            Client_List;
        false ->
            Client_ID ! {connect, client_joined},
            [{Client_ID, Name} | Client_List]        %add user to the list
    end.

remove_client(Client_ID, Client_List) -> 
    lists:keydelete(Client_ID, 1, Client_List).