-module(user_default).

-export([help/0]).

-export([p/1, p/2]).
-export([pn/1, pn/2]).
-export([yy/1, yp/1]).
-export([reloader/0]).

-export([trace/1, trace/2]).
-export([tracen/1, tracen/2]).
-export([trace_off/0]).
-export([untrace/1, untrace/2]).

%% output helpers for shell
p(String) -> io:format(String).
p(String, Args) -> io:format(String, Args).
pn(String) -> p(String ++ "~n").
pn(String, Args) -> p(String ++ "~n", Args).
yy(Term) -> pn("~w", [Term]).
yp(Term) -> pn("~p", [Term]).
reloader() -> sync:go().

%% +-----------------------------------------------------------------+
%% | NESTED TRACER                                                   |
%% +-----------------------------------------------------------------+

nested_tracer() ->
    dbg:tracer(process, {fun nested_trace/2, 0}).

nested_trace({trace, Pid, call, {Mod, Fun, Args}}, Level) ->
    pn("~p ~s~p:~p~s", [Pid, fill_spaces(Level), Mod, Fun, format_args(Args)]),
    Level + 1;
nested_trace({trace, Pid, return_from, {_, _, _}, ReturnValue}, Level) ->
    NewLevel = Level - 1,
    pn("~p ~s~p", [Pid, fill_spaces(NewLevel), ReturnValue]),
    NewLevel;
nested_trace(Any, Level) ->
    pn("trace_msg: ~p", [Any]),
    Level.

tracer() ->
    dbg:tracer(process, {fun flat_trace/2, 0}).

flat_trace({trace, Pid, call, {Mod, Fun, Args}}, Level) ->
    pn("call: ~p ~p:~p~s, level: ~w", [Pid, Mod, Fun, format_args(Args), Level]),
    Level + 1;
flat_trace({trace, Pid, return_from, {Mod, Fun, _}, ReturnValue}, Level) ->
    NewLevel = Level - 1,
    pn("retn: ~p ~p:~p -> ~p, level: ~w", [Pid, Mod, Fun, ReturnValue, NewLevel]),
    NewLevel;
flat_trace(Any, Level) ->
    pn("trace_msg: ~p", [Any]),
    Level.

fill_spaces(Level) ->
    lists:duplicate(Level, "| ").

format_args(Args) ->
    Args1 = io_lib:format("~w", [Args]),
    Opts = [global, {return, list}],
    Args2 = re:replace(Args1, "^\\[", "(", Opts),
    Args3 = re:replace(Args2, "\\]$", ")", Opts),
    re:replace(Args3, "\\,", ", ", Opts).

%% +-----------------------------------------------------------------+
%% | TRACING HELPERS                                                 |
%% +-----------------------------------------------------------------+

trace(Module) ->
    tracer(), setup_tracing(Module).
trace(Module, Function) ->
    tracer(), setup_tracing(Module, Function).
tracen(Module) ->
    nested_tracer(), setup_tracing(Module).
tracen(Module, Function) ->
    nested_tracer(), setup_tracing(Module, Function).

trace_off() -> dbg:stop().

untrace(Module) ->
    {ok, _} = dbg:ctpl(Module), ok.
untrace(Module, Function) ->
    {ok, _} = dbg:ctpl(Module, Function), ok.

setup_tracing(Module)
  when is_atom(Module) ->
    {ok, _} = dbg:p(all, call),
    {ok, _} = dbg:tpl(Module, tracing_options()),
    ok.
setup_tracing(Module, Function)
  when is_atom(Module),
       is_atom(Function) ->
    {ok, _} = dbg:p(all, call),
    {ok, _} = dbg:tpl(Module, Function, tracing_options()),
    ok.

tracing_options() ->
    [{'_', [], [{return_trace}, {exception_trace}]}].

%% +-----------------------------------------------------------------+
%% | HELP MENU                                                       |
%% +-----------------------------------------------------------------+
help() ->
    shell_default:help(),
    pn("~n** commands in module ~s (user helpers) **~n", [?MODULE]),
    pn("reloader()                -- run code reloader"),
    pn("trace(Module)             -- run tracing for 'Module'"),
    pn("trace(Module, Function)   -- run tracing for 'Function' of 'Module'"),
    pn("tracen(Module)            -- run tracing for 'Module' - nested way"),
    pn("tracen(Module, Function)  -- run tracing for 'Function' of 'Module' - nasted way"),
    pn("trace_off()               -- disable tracing"),
    pn("untrace(Module)           -- stop tracing 'Module'"),
    pn("untrace(Module, Function) -- stop tracing 'Function' of 'Module'"),
    pn("p(String)                 -- print string"),
    pn("p(String, Args)           -- print string with format"),
    pn("pn(String)                -- print string with ~~n"),
    pn("pn(String, Args)          -- print string with ~~n and format"),
    pn("yy(Term)                  -- print raw term"),
    pn("yp(Term)                  -- print term").
