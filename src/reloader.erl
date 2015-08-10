%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([all_changed/0, all_changed/1]).
-export([is_changed/1]).
-export([reload/1]).
-record(state, {
    last,
    tref,
    timeout
}).

%% External API

%% @doc Start the reloader.
start() ->
    application:start(reloader).

%% @doc Stop the reloader.
stop() ->
    application:stop(reloader).

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

%% @private
init([]) ->
    Timeout = application:get_env(reloader, timeout, timer:seconds(1)),
    %% immediately reload on startup
    reload(all_changed()),
    TRef = erlang:send_after(Timeout, self(), doit),
    {ok, #state{last = stamp(), tref = TRef, timeout = Timeout}}.

%% @private
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @private
handle_cast(_Req, State) ->
    {noreply, State}.

%% @private
handle_info(doit, State) ->
    Now = stamp(),
    _ = doit(State#state.last, Now),
    TRef = erlang:send_after(State#state.timeout, self(), doit),
    {noreply, State#state{last = Now, tref = TRef}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.tref),
    ok.


%% @private
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

-spec all_changed() -> [module()].
%% @doc Return a list of loaded beam modules that have changed.
all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

-spec all_changed(atom()) -> [module()].
%% @doc Return a list of loaded beam modules that have changed in application App.
all_changed(App) ->
    ensure_loaded(App),
    {ok, Modules} = application:get_key(App, modules),
    [M || M <- Modules, is_changed(M)].

-spec is_changed(module()) -> boolean().
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
        false
    end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

doit(From, To) ->
    [case file:read_file_info(Filename) of
        {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
            reload(Module);
        {ok, _} ->
            unmodified;
        {error, enoent} ->
            %% The Erlang compiler deletes existing .beam files if
            %% recompiling fails.  Maybe it's worth spitting out a
            %% warning here, but I'd want to limit it to just once.
            gone;
        {error, Reason} ->
            io:format("Error reading ~s's file info: ~p~n",
                [Filename, Reason]),
            error
    end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

-spec reload(module() | [module()]) -> {module(), Result} | [{module(), Result}] when
    Result :: unchanged
    | reload
    | reload_but_test_failed
    | error.
%% @doc code:purge/1 and code:load_file/1.
reload(Modules) when is_list(Modules) ->
    lists:map(fun reload/1, Modules);
reload(Module) ->
    io:format("Reloading ~p ...", [Module]),
    LoadRet =
        case is_changed(Module) of
            false ->
                io:format(" unchanged.~n"),
                unchanged;
            true ->
                code:purge(Module),
                case code:load_file(Module) of
                    {module, Module} ->
                        io:format(" ok.~n"),
                        case erlang:function_exported(Module, test, 0) of
                            true ->
                                io:format(" - Calling ~p:test() ...", [Module]),
                                case catch Module:test() of
                                    ok ->
                                        io:format(" ok.~n"),
                                        reload;
                                    Reason ->
                                        io:format(" fail: ~p.~n", [Reason]),
                                        reload_but_test_failed
                                end;
                            false ->
                                reload
                        end;
                    {error, Reason} ->
                        io:format(" fail: ~p.~n", [Reason]),
                        error
                end
        end,
    {Module, LoadRet}.

ensure_loaded(App) ->
    L = application:loaded_applications(),
    case lists:keyfind(App, 1, L) of
        false ->
            ok = application:load(App);
        _ ->
            ok
    end.

stamp() ->
    erlang:localtime().

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
