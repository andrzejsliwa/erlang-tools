%% See LICENSE for licensing information.
-module({{name}}).

%% Interface exports
-export([main/1]).

%% Command line helper macro
%%-define(CMD, filename:basename(escript:script_name())).

%% +-----------------------------------------------------------------+
%% | INTERFACE FUNCTIONS                                             |
%% +-----------------------------------------------------------------+

-spec main(Args :: [string()]) -> ok.
main(_Args)->
    ok.

%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+

%% @private