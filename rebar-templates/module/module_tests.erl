%% See LICENSE for licensing information.
-module({{name}}_tests).

%% Must be declared before include eunit.hrl (?LET conflict)
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MODNAME, {{name}}).

%% makes compiler shut up.
-spec test() -> ok.

%% +-----------------------------------------------------------------+
%% | TEST GENERATORS - EUnit                                           |
%% +-----------------------------------------------------------------+

-spec {{name}}_eunits_test_() -> [term()].
{{name}}_eunits_test_() ->
    %% define your asserts in the returned list, e.g.:
    %% [
    %%  ?_assert(?MODNAME:main([]) =:= ok),
    %%  ?_assert(not (1 =:= 2)),
    %%  ?_assertNot(1 =:= 2)
    %%  ?_assertMatch(ok, ?MODNAME:main([])),
    %%  ?_assertEqual(ok, ?MODNAME:main([])),
    %%  ?_assertError(undef, ?MODNAME:main(0, 1)),
    %%  ?_assertExit(normal, exit(normal)),
    %%  ?_assertException(throw, {not_found,_}, throw({not_found,42})),
    %%  ?_assertThrow({not_found, _}, throw({not_found, 1}))
    %%  ?_assertCmd("ls"),
    %%  ?_assertCmdStatus(N, CommandString)
    %%  ?_assertCmdOutput("1\n", "echo '1'")
    %% ].
    [].

%% +-----------------------------------------------------------------+
%% | TEST GENERATORS - PropEr                                             |
%% +-----------------------------------------------------------------+

-spec {{name}}_proper_test_() -> [term()].
{{name}}_proper_test_() ->
    %% define your proper tests
    %% [{timeout, 30,
    %%  {"PropEr tests", ?_test(run_propers())}}].
    [].


%% +-----------------------------------------------------------------+
%% | RUN PropEr                                            |
%% +-----------------------------------------------------------------+

run_propers() ->
    ?assertEqual(true, proper:check_spec(
                         {?MODNAME, main, 1}, proper_formating())).


%% +-----------------------------------------------------------------+
%% | HELPERS                                            |
%% +-----------------------------------------------------------------+

proper_formating() ->
    Print = fun(Format, Data) ->
                    io:format(standard_error, Format, Data)
            end,
    [{'on_output', Print}, {numtests, 100}].
