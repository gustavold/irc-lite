%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_server).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [delete/2, foreach/2, map/2, member/2,reverse/2]).

-compile(export_all).


start() -> start("chat.conf").

start(ConfigFile) ->
    start_server(),
    lib_chan:start_server(ConfigFile).

start_server() ->
    register(chat_server, 
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Val= (catch server_loop([],[])),
			   io:format("Server terminated with:~p~n",[Val])
		   end)).



server_loop(L,Ns) ->
    receive
        {groups, C} ->
	     lib_chan_mm:send(C, {groups, [G || {G, _} <- L]}),
	     server_loop(L, Ns);
        {mm, Channel, {groups}} ->
	     lib_chan_mm:send(Channel, {groups, [G || {G, _} <- Ns]}),
	     server_loop(L, Ns);
	{mm, Channel, {login, Group, Nick}} ->
	    case lookup(Group, L) of
		{ok, Pid} ->
		    Pid ! {login, Channel, Nick},
		    server_loop(L, Ns);
		error ->
		    Pid = spawn_link(fun() ->
					     chat_group:start(Channel, Nick) 
				     end),
		    server_loop([{Group,Pid}|L], Ns)
	    end;
	{mm, _Channel, {register_group, Group, Addr}} ->
	    server_loop(L, [{Group, Addr}|Ns]);
	{mm, Channel, {lookup, Group}} ->
	     case lookup(Group, Ns) of
	         {ok, Host} -> lib_chan_mm:send(Channel, {lookup, Group, Host});
		 error -> lib_chan_mm:send(Channel, {lookup, Group, notfound})
	    end,
	    server_loop(L, Ns);
	{mm_closed, _} ->
	    server_loop(L, Ns); 
	{'EXIT', Pid, allGone} ->
	    L1 = remove_group(Pid, L),
	    server_loop(L1, Ns);
	Msg ->
	    io:format("Server received Msg=~p~n",
		      [Msg]),
	    server_loop(L, Ns)
    end.



lookup(G, [{G,Pid}|_]) -> {ok, Pid};
lookup(G, [_|T])       -> lookup(G, T);
lookup(_,[])           -> error.

remove_group(Pid, [{G,Pid}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T])       -> [H|remove_group(Pid, T)];
remove_group(_, [])            -> [].

