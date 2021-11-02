% Test module
-module(test).

-export([hello/0]).

%% Hello function
%% Hello commit @ tester

%% Hello commit  tab
-spec hello() -> ok.
hello() ->
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
