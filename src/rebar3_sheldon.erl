%%% @doc Main entry point for the rebar3 rebar3_sheldon plugin.
-module(rebar3_sheldon).

-export([init/1]).

-ignore_xref([init/1]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_sheldon_prv:init(State),
    {ok, State1}.
