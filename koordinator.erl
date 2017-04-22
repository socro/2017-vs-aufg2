-module(koordinator).
-export([start/0]).

-import(helper,[logHeader/1]).
-import(werkzeug,[logging/2,get_config_value/2]).
-import(lists,[concat/1]).

start() ->
  %% Read koordinator.cfg
  KConfig = "cfg/koordinator.cfg",

  {ok, ConfigListe}     = file:consult(KConfig),
  {ok, Arbeitszeit}     = get_config_value(arbeitszeit,ConfigListe),
  {ok, Termzeit}        = get_config_value(termzeit,ConfigListe),
  {ok, GGTAnz}          = get_config_value(ggtprozessnummer,ConfigListe),
  {ok, NameserviceNode} = get_config_value(nameservicenode,ConfigListe),
  {ok, NameserviceName} = get_config_value(nameservicename,ConfigListe),
  {ok, KoordName}       = get_config_value(koordinatorname,ConfigListe),
  {ok, Quote}           = get_config_value(quote,ConfigListe),
  {ok, HelpFlag}        = get_config_value(korrigieren,ConfigListe),


  %% Pre-flight initialisation
  MyPID       = self(),

  LoggingDir  = "logs/koordinator/",
  LoggingFile = "koordinator.log",
  Log         = LoggingDir ++ LoggingFile,

  logging(Log,concat(["-----------------------------------Log File Koordinator-----------------------------------\n"])),
  logging(Log,concat([logHeader(MyPID),"Koordinator gestartet.\n"])),
  eof.



%# code Schnipsel
%setNeighbors(ErsterProzess|R) ->
%% sende nachricht für ersten prozess
%% sende nachricht für letzten prozess
%setNeighbors(LeftNeighbor,[Elem|[RightNeigbour|RestRing])
%setNeighbors(LeftNeighbor,[Elem|[RightNeigbour|RestRing]) ->
%% sende nachricht für alle anderen Prozesse