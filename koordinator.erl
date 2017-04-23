-module(koordinator).
-export([start/0]).

-import(helper,[logHeader/1,lookup/1,shuffle/1,notify_nameservice/3]).
-import(werkzeug,[logging/2,get_config_value/2,bestimme_mis/2]).
-import(io_lib,[format/2]).
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
  {ok, QuoteProzent}    = get_config_value(quote,ConfigListe),
  {ok, HelpFlag}        = get_config_value(korrigieren,ConfigListe),

  Nameservice = {NameserviceName, NameserviceNode},
  %% Pre-flight initialisation

  LoggingDir  = "logs/koordinator/",
  LoggingFile = "koordinator.log",
  Log         = LoggingDir ++ LoggingFile,

  StaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,QuoteProzent,HelpFlag],

  logging(Log,concat(["-----------------------------------Log File Koordinator-----------------------------------\n"])),
  logging(Log,concat([logHeader(self()),"Koordinator gestartet.\n"])),
  register(KoordName,self()),
  net_adm:ping(NameserviceNode),
  notify_nameservice(Log,KoordName,Nameservice),
  timer:sleep(500),
  loop(StaticConfig,0,notset,[],false),
  logging(Log,concat([logHeader(self()),"Koordinator beendet.\n"]))
  .

setNeighbors(Log,ProzessListe) ->
  [ErsterProzessName|[ZweiterProzessName|R]] = ProzessListe,
  {_,[VorletzterProzessName,LetzerProzessName]} = lists:split(length(ProzessListe)-2,ProzessListe),
  ErsterProzess = lookup(ErsterProzessName),
  LetzerProzess = lookup(LetzerProzessName),
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~p.\n",[logHeader(self()),LetzerProzessName,ZweiterProzessName,ErsterProzess])),
  ErsterProzess ! {setneighbors,LetzerProzessName,ZweiterProzessName},
  LetzerProzess ! {setneighbors,VorletzterProzessName,ErsterProzessName},
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~p.\n",[logHeader(self()),VorletzterProzessName,ErsterProzessName,LetzerProzess])),
  setNeighbors(Log,ErsterProzessName,[ZweiterProzessName|R]).
setNeighbors(Log,LeftNeighborName,[ProzessName,RightNeighborName]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~p.\n",[logHeader(self()),LeftNeighborName,RightNeighborName,ProzessName])),
  Prozess ! {setneighbors,LeftNeighborName,RightNeighborName};
setNeighbors(Log,LeftNeighborName,[ProzessName|[RightNeighborName|R]]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~p.\n",[logHeader(self()),LeftNeighborName,RightNeighborName,ProzessName])),
  Prozess ! {setneighbors,LeftNeighborName,RightNeighborName},
  setNeighbors(Log,ProzessName,[RightNeighborName|R]).

sendMis(Log,[],[],_MsgType) ->
  logging(Log,format("~sAlle PMs gesetzt",[logHeader(self())]));
sendMis(Log,[ProzessName|RProzesse],[Mi|RMi],MsgType) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende setpm mit Mi ~p an ~p.\n",[logHeader(self()),Mi,ProzessName])),
  Prozess ! {MsgType,Mi},
  sendMis(Log,RProzesse,RMi,MsgType).

sendStartMis(Log,Wggt,ProzessListe) ->
  AnzahlStarterMisTmp = round(length(ProzessListe) * 20 / 100),
  if
    AnzahlStarterMisTmp < 2 -> AnzahlStarterMis = 2;
    AnzahlStarterMisTmp >= 2 -> AnzahlStarterMis = AnzahlStarterMisTmp
  end,
  {StarterMisListeTmp,_} = lists:split(AnzahlStarterMis,ProzessListe),
  % Shuffle StarterMisListeTmp
  StarterMisListe = shuffle(StarterMisListeTmp),
  StarterMiWerteListe = bestimme_mis(Wggt,length(StarterMisListe)),
  sendMis(Log,StarterMisListe,StarterMiWerteListe,sendy).

sendKill(Log,[]) ->
  logging(Log,format("~sAllen ggt-Prozesse kill gesendet",[logHeader(self())]));
sendKill(Log,[ProzessName|R]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende kill an ggt-Prozess ~p",[logHeader(self()),ProzessName])),
  Prozess ! kill,
  sendKill(Log,R).

sendTellmi(Log,[]) ->
  logging(Log,format("~sAllen Prozesse tellmi gesendet",[logHeader(self())]));
sendTellmi(Log,[ProzessName|RProzesse]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende tellmi an ~p.\n",[logHeader(self()),ProzessName])),
  Prozess ! {self(),tellmi},
  sendTellmi(Log,RProzesse).

sendPingGGT(Log,[]) ->
  logging(Log,format("~sAllen Prozesse pingGGT gesendet",[logHeader(self())]));
sendPingGGT(Log,[ProzessName|RProzesse]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende pingGGT an ~p.\n",[logHeader(self()),ProzessName])),
  Prozess ! {self(),pingGGT},
  sendPingGGT(Log,RProzesse).

loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase) ->
  [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,Quote,HelpFlag] = StaticConfig,
  receive
    % public messages
    {From,getsteeringval} when Arbeitsphase == false ->
      GGTAnzahlGemeldetNeu = GGTAnzahlGemeldet + GGTAnz,
      QuoteAbsolut = round(GGTAnzahlGemeldetNeu * Quote / 100),
      From ! {steeringval,Arbeitszeit,Termzeit,QuoteAbsolut,GGTAnz},
      loop(StaticConfig,GGTAnzahlGemeldetNeu,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {From,getsteeringval} when Arbeitsphase == true ->
      logging(Log,format("~sgetsteeringval in Arbeitsphase erhalten, ignorieren...\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {hello,Clientname} when Arbeitsphase == false ->
      GGTListeNeu = concat([GGTListe,Clientname]),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListeNeu,Arbeitsphase)
      ;
    {hello,Clientname} when Arbeitsphase == true ->
      logging(Log,format("~shello in Arbeitsphase erhalten, ignorieren...\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {briefmi,{Clientname,CMi,CZeit}} when Arbeitsphase == true,  AktuellKleinsterGGT /= notset, CMi < AktuellKleinsterGGT ->
      loop(StaticConfig,GGTAnzahlGemeldet,CMi,GGTListe,Arbeitsphase)
      ;
    {briefmi,{Clientname,CMi,CZeit}} when Arbeitsphase == false; CMi >= AktuellKleinsterGGT ->
      logging(Log,format("~sbriefmi ignorieren...\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {From,briefterm,{Clientname,CMi,CZeit}} when Arbeitsphase == true ->
      logging(Log,format("~sbriefmi mit Mi ~p von ~p erhalten, Zeit ~p.\n",[logHeader(self()),CMi,Clientname,CZeit])),
      if
        CMi > AktuellKleinsterGGT ->
          logging(Log,format("~sMi ist zu groß",[logHeader(self())])),
          if
            HelpFlag == true -> From ! {sendy,AktuellKleinsterGGT}
          end
      end,
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {From,briefterm,{Clientname,CMi,CZeit}} when Arbeitsphase == false ->
      logging(Log,format("~sbriefterm ignorieren da außerhalb der Arbeitsphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {calc,WggT} when Arbeitsphase == true->
      MiWerteListe = bestimme_mis(WggT,length(GGTListe)),
      sendMis(Log,GGTListe,MiWerteListe,setpm),
      sendStartMis(Log,WggT,GGTListe),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {calc,WggT} when Arbeitsphase == false->
      logging(Log,format("~scalc ignorieren da außerhalb der Arbeitsphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {pongGGT,Clientname} ->
      logging(Log,format("~spongGGT erhalten von ~p\n",[logHeader(self()),Clientname])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    reset ->
      sendKill(Log,GGTListe),
      logging(Log,format("~sKoordinator reset\n",[logHeader(self())])),
      loop(StaticConfig,0,notset,[],false)
      ;
    step when Arbeitsphase == false ->
      GGTProzessListeShuffled = shuffle(GGTListe),
      setNeighbors(Log,GGTProzessListeShuffled),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,true)
      ;
    step when Arbeitsphase == true ->
      logging(Log,format("~sstep ignorieren da außerhalb der Initphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    prompt when Arbeitsphase == true ->
      sendTellmi(Log,GGTListe),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    prompt when Arbeitsphase == false ->
      logging(Log,format("~sprompt ignorieren da außerhalb der Arbeitsphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    kill ->
      sendKill(Log,GGTListe),
      logging(Log,format("~sKoordinator kill\n",[logHeader(self())]))
      ;
    nudge ->
      sendPingGGT(Log,GGTListe),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    toggle when HelpFlag == true ->
      logging(Log,format("~stoggle helpFlag von true auf false\n",[logHeader(self())])),
      NeuStaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,Quote,false],
      loop(NeuStaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    toggle when HelpFlag == false ->
      logging(Log,format("~stoggle helpFlag von false auf true\n",[logHeader(self())])),
      NeuStaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,Quote,true],
      loop(NeuStaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    Any ->
      logging(Log,format("~sHiermit können wir NICHTS anfangen: ~p\n",[logHeader(self()),Any])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
  end.


