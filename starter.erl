-module(starter).
-export([start/1]).

-import(helper,[logHeader/1,logging/3]).
-import(werkzeug,[logging/2,get_config_value/2]).
-import(lists,[concat/1]).

start(Starternummer) ->
  %% Read ggt.cfg
  GgtConfig = "cfg/ggt.cfg",

  {ok, ConfigListe}     = file:consult(GgtConfig),
  {ok, Praknummer}      = get_config_value(praktikumsgruppe, ConfigListe),
  {ok, Teamnummer}      = get_config_value(teamnummer,       ConfigListe),
  {ok, NameserviceNode} = get_config_value(nameservicenode,  ConfigListe),
  {ok, NameserviceName} = get_config_value(nameservicename,  ConfigListe),
  {ok, KoordName}       = get_config_value(koordinatorname,  ConfigListe),

  GgtConfigList = [Starternummer,Praknummer,Teamnummer,NameserviceNode,NameserviceName,KoordName],

  %% Pre-flight initialisation
  MyPID       = self(),
  StarterName = concat(["starter",Teamnummer,Starternummer]),

  LoggingDir  = "logs/starters/",
  LoggingFile = concat(["starter",Starternummer,".log"]),
  Log         = LoggingDir ++ LoggingFile,

  logging(Log,concat(["-----------------------------------Log File Starter ",Teamnummer,Starternummer, "-----------------------------------\n"])),



  %% Register on local erlang node, nameservice and wait
  register(list_to_atom(StarterName),self()),
  net_adm:ping(NameserviceNode),
  timer:sleep(500),

  %% Get koordinator PID
  {NameserviceName,NameserviceNode} ! {self(),{lookup,KoordName}},
  receive
    {pin,{KName,KNode}} -> logging(Log,concat([logHeader(MyPID),"Koordinator zu erreichen unter: {",KName,",",KNode,"}\n"])),
                           getSteeringVal({KName,KNode},GgtConfigList,Log);
    not_found           -> logging(Log,concat([logHeader(MyPID),"Nameservice konnte Koordinator nicht finden, beende Starter..\n"]))
  end,
  logging(Log,concat([logHeader(MyPID),"Starter hat seine Arbeit beendet.\n"])).

getSteeringVal(Koordinator,GgtConfigList,Log) ->
  Koordinator ! {self(),getsteeringval},
  receive
    {steeringval,ArbeitsZeit,TermZeit,Quota,GGTProzessanz} ->
      logging(Log,concat([logHeader(self()),"SteeringVal von Koordinator erhalten\n"])),
      logging(Log,concat([logHeader(self()),"ArbeitsZeit = ",ArbeitsZeit,"\n"])),
      logging(Log,concat([logHeader(self()),"TermZeit = ",TermZeit,"\n"])),
      logging(Log,concat([logHeader(self()),"Quota = ",Quota,"\n"])),
      logging(Log,concat([logHeader(self()),"GGTProzessanz = ",GGTProzessanz,"\n"])),
      logging(Log,"ggt:start([ArbeitsZeit,TermZeit,Quota,GGTProzessanz,Starternummer,Praknummer,Teamnummer,NameserviceNode,NameserviceName,KoordName])"),
      spawn_ggt(ArbeitsZeit,TermZeit,Quota,GGTProzessanz,GgtConfigList,Log)
  end.

spawn_ggt(_,_,_,0,_,Log) -> logging(Log,concat([logHeader(self()),"Alle ggt-Prozesse gestartet.\n"]));
spawn_ggt(ArbeitsZeit, TermZeit, Quota, GGTProzessanz, GgtConfigList,Log) ->
  [Starternummer,Praknummer,Teamnummer,NameserviceNode,NameserviceName,KoordName] = GgtConfigList,
  spawn(ggt,start,[[ArbeitsZeit,TermZeit,Quota,GGTProzessanz,Starternummer,Praknummer,Teamnummer,NameserviceNode,NameserviceName,KoordName]]),
  logging(Log,io_lib:format("ggt:start([~p,~p,~p,~p,~p,~p,~p,~p,~p,~p])\n",
    [ArbeitsZeit,TermZeit,Quota,GGTProzessanz,Starternummer,Praknummer,Teamnummer,NameserviceNode,NameserviceName,KoordName])),
  spawn_ggt(ArbeitsZeit, TermZeit, Quota, GGTProzessanz-1, GgtConfigList, Log).