-module(helper).
-import(werkzeug,[get_config_value/2,logging/2]).
-import(io_lib,[format/2]).
-export([del_dir/1,logHeader/1,shuffle/1,lookup/1,notify_nameservice/3,logging/3]).

%% Source: http://stackoverflow.com/a/30611957
%% Added check if directory exists
del_dir(Dir) ->
  case file:list_dir(Dir) of
    {error, _Reason} -> ok;
    {ok, _Reason} -> lists:foreach(fun(D) ->
      ok = file:del_dir(D)end,
      del_all_files([Dir], []))
  end.

del_all_files([], EmptyDirs) ->
  EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
  {ok, FilesInDir} = file:list_dir(Dir),
  {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
    Path = Dir ++ "/" ++ F,
    case filelib:is_dir(Path) of
      true ->
        {Fs, [Path | Ds]};
      false ->
        {[Path | Fs], Ds}
    end
                              end, {[],[]}, FilesInDir),
  lists:foreach(fun(F) ->
    ok = file:delete(F)
                end, Files),
  del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

%% Einheitlicher Log Header
logHeader(PID) ->
  [_Head|ShorterPID] = pid_to_list(PID),
  lists:concat(["<",node(),"|",werkzeug:timeMilliSecond(),ShorterPID," "]).

lookup(WhatName) ->
  Log = "logs/lookup.log",
  KConfig = "cfg/koordinator.cfg",
  {ok, ConfigListe}     = file:consult(KConfig),
  {ok, NameserviceNode} = get_config_value(nameservicenode,ConfigListe),
  {ok, NameserviceName} = get_config_value(nameservicename,ConfigListe),
  Nameservice = {NameserviceName,NameserviceNode},
  Nameservice ! {self(),{lookup,WhatName}},
  receive
    not_found ->
      logging(Log,format("~sLookup ~p... not found\n",[logHeader(self()),WhatName]));
    {pin,{WhatName,WhatNode}} ->
      logging(Log,format("~sLookup ~p... ok ({~p,~p})\n",[logHeader(self()),WhatName,WhatName,WhatNode])),
      {WhatName, WhatNode}
  end.

shuffle(Liste) ->
  % Quelle: http://stackoverflow.com/a/8820501
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Liste])].

notify_nameservice(Log,Name,Nameservice) ->
  Nameservice ! {self(),{rebind,Name,node()}},
  receive
    ok -> logging(Log,format("~srebind ok\n",[logHeader(self())]))
  end.

logging(LogFile, String, critical) ->
  werkzeug:logging(LogFile,String),
  CriticalLog = lists:concat(["logs/",werkzeug:getUTC(),".log"]),
  werkzeug:logging(CriticalLog,String).