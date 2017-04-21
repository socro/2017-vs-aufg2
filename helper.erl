-module(helper).
-export([del_dir/1,logHeader/1]).

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
  lists:concat(["<",node(),">|",werkzeug:timeMilliSecond(),">",pid_to_list(PID)]).