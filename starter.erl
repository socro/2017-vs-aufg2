-module(starter).
-export([start_starter/2]).

-import(helper,[logHeader/1]).
-import(werkzeug,[logging/2,get_config_value/2]).
-import(lists,[concat/1]).

start_starter(Anzahl,Starternummer) ->
  %% Read ggt.cfg
  GgtConfig = "cfg/ggt.cfg",

  {ok, ConfigListe}     = file:consult(GgtConfig),
  {ok, Praknummer}      = get_config_value(praktikumsgruppe, ConfigListe),
  {ok, Teamnummer}      = get_config_value(teamnummer,       ConfigListe),
  {ok, NameserviceNode} = get_config_value(nameservicenode,  ConfigListe),
  {ok, NameserviceName} = get_config_value(nameservicename,  ConfigListe),
  {ok, KoordName}       = get_config_value(koordinatorname,  ConfigListe),

  %% Pre-flight initialisation
  StarterName = concat(["starter",Teamnummer,Starternummer]),

  LoggingDir  = "logs/starters/",
  LoggingFile = concat(["starter",Starternummer,".log"]),
  Log         = LoggingDir ++ LoggingFile,

  logging(Log,concat([logHeader(self()),"Starter ",Teamnummer,Starternummer, " gestartet.\n"])),
  logging(Log,concat([logHeader(self()),"Es werden ",Anzahl," ggt-Prozesse gestartet.\n"])),


  %% Register on local erlang node, nameservice and wait
  register(StarterName,self()),
  net_adm:ping(NameserviceNode),
  timer:sleep(500),


  nameservice:
.