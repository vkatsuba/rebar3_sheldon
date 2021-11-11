% Test module
-module(test).

-export([hello/0]).

%% Hello function
%% Hello commit @ tester

%% Hello commit  tab
-spec hello() -> ok.
hello() ->
    MultiString = "a " "string " "with " "multiple " "pieces",
    MultiBinary = <<"a " "binary " "with " "multiple " "pieces">>,
    AnotherMultiBinary =
        <<"a ", "binary ", "with ", "multiple ", "pieces ", "separated ", "by ", "commas ">>,
    EscapedString = "a string with \"quotes\"",
    HelloWorld = "Hello
      World!",
    io_format("~p~n", [HelloWorld]),
    HelloWorldBin = <<"Hello World Binary!">>,
    BinInt = <<1:8>>, % Inline comment
    io_format("~p~n", [HelloWorldBin]),
    %% @test Comment
    io_format("Done
    ~n").

io_format(_, Text) ->
    Text.
