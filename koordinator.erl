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
  {ok, QuoteProzent}           = get_config_value(quote,ConfigListe),
  {ok, HelpFlag}        = get_config_value(korrigieren,ConfigListe),

  Nameservice = {NameserviceName, NameserviceNode},
  %% Pre-flight initialisation
  MyPID       = self(),

  LoggingDir  = "logs/koordinator/",
  LoggingFile = "koordinator.log",
  Log         = LoggingDir ++ LoggingFile,

  StaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,QuoteProzent,HelpFlag],

  logging(Log,concat(["-----------------------------------Log File Koordinator-----------------------------------\n"])),
  logging(Log,concat([logHeader(MyPID),"Koordinator gestartet.\n"])),

  net_adm:ping(NameserviceNode),
  register(KoordName,self())
  .

loop(StaticConfig,GGTAnzahlGemeldet,GGTListe,Arbeitsphase) ->
  [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,Quote,HelpFlag] = StaticConfig,
  receive
    % public messages
    {From,getsteeringval} when Arbeitsphase == false ->
      GGTAnzahlGemeldetNeu = GGTAnzahlGemeldet + GGTAnz,
      QuoteAbsolut = round(GGTAnzahlGemeldetNeu * Quote / 100),
      From ! {steeringval,Arbeitszeit,Termzeit,QuoteAbsolut,GGTAnz}
      ;
    {From,getsteeringval} when Arbeitsphase == true ->
      logging(Log,format("~sgetsteeringval in Arbeitsphase erhalten, ignorieren...\n",[logHeader(self())]));
    {hello,Clientname} when Arbeitsphase == false ->
      GGTListeNeu = concat([GGTListe,Clientname])
      ;
    {hello,Clientname} when Arbeitsphase == true ->
      logging(Log,format("~shello in Arbeitsphase erhalten, ignorieren...\n",[logHeader(self())]));
    {briefmi,{Clientname,CMi,CZeit}} ->
      ;
    {From,briefterm,{Clientname,CMi,CZeit}} ->
      ;
    {calc,WggT} ->
      ;
    reset ->
      ;
    step ->
      ;
    prompt ->
      ;
    kill ->
      ;
    nudge ->
      ;
    toggle ->
      ;
  end.


%# code Schnipsel
%setNeighbors(ErsterProzess|R) ->
%% sende nachricht für ersten prozess
%% sende nachricht für letzten prozess
%setNeighbors(LeftNeighbor,[Elem|[RightNeigbour|RestRing])
%setNeighbors(LeftNeighbor,[Elem|[RightNeigbour|RestRing]) ->
%% sende nachricht für alle anderen Prozesse