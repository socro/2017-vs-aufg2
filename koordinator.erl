-module(koordinator).
-export([start/0]).

-import(helper,[logHeader/1,lookup/1,shuffle/1,notify_nameservice/3,logging/3]).
-import(werkzeug,[logging/2,get_config_value/2,bestimme_mis/2,now2string/1]).
-import(io_lib,[format/2]).
-import(lists,[append/2,concat/1]).

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
  LoggingFile = concat([node(),".log"]),
  Log         = LoggingDir ++ LoggingFile,

  StaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,QuoteProzent,HelpFlag],

  logging(Log,concat(["-----------------------------------Log File Koordinator-----------------------------------\n"])),
  logging(Log,concat([logHeader(self()),"Koordinator gestartet.\n"]),critical),
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
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~w.\n",
    [logHeader(self()),LetzerProzessName,ZweiterProzessName,ErsterProzess])),
  ErsterProzess ! {setneighbors,LetzerProzessName,ZweiterProzessName},
  LetzerProzess ! {setneighbors,VorletzterProzessName,ErsterProzessName},
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~w.\n",
    [logHeader(self()),VorletzterProzessName,ErsterProzessName,LetzerProzess])),
  setNeighbors(Log,ErsterProzessName,[ZweiterProzessName|R]).
setNeighbors(Log,LeftNeighborName,[ProzessName,RightNeighborName]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~w.\n",
    [logHeader(self()),LeftNeighborName,RightNeighborName,Prozess])),
  Prozess ! {setneighbors,LeftNeighborName,RightNeighborName};
setNeighbors(Log,LeftNeighborName,[ProzessName|[RightNeighborName|R]]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende setneighbors mit links ~p und rechts ~p an ~w.\n",
    [logHeader(self()),LeftNeighborName,RightNeighborName,Prozess])),
  Prozess ! {setneighbors,LeftNeighborName,RightNeighborName},
  setNeighbors(Log,ProzessName,[RightNeighborName|R]).

sendMis(Log,[],[],_MsgType) ->
  logging(Log,format("~sAlle PMs gesetzt\n",[logHeader(self())]));
sendMis(Log,[ProzessName|RProzesse],[Mi|RMi],MsgType) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende setpm mit Mi ~p an ~p.\n",[logHeader(self()),Mi,ProzessName])),
  Prozess ! {MsgType,Mi},
  sendMis(Log,RProzesse,RMi,MsgType).

sendStartMis(Log,Wggt,ProzessListe) ->
  GeshuffelteProzessliste = shuffle(ProzessListe),
  AnzahlStarterMisTmp = round(length(ProzessListe) * 20 / 100),
  if
    AnzahlStarterMisTmp < 2 -> AnzahlStarterMis = 2;
    AnzahlStarterMisTmp >= 2 -> AnzahlStarterMis = AnzahlStarterMisTmp
  end,
  {StarterMisListe,_} = lists:split(AnzahlStarterMis,GeshuffelteProzessliste),
  StarterMiWerteListe = bestimme_mis(Wggt,length(StarterMisListe)),
  sendMis(Log,StarterMisListe,StarterMiWerteListe,sendy).

sendKill(Log,[]) ->
  logging(Log,format("~sAllen ggt-Prozesse kill gesendet\n",[logHeader(self())]));
sendKill(Log,[ProzessName|R]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende kill an ggt-Prozess ~p",[logHeader(self()),ProzessName])),
  Prozess ! kill,
  sendKill(Log,R).

sendTellmi(Log,[]) ->
  logging(Log,format("~sAllen Prozesse tellmi gesendet\n",[logHeader(self())]));
sendTellmi(Log,[ProzessName|RProzesse]) ->
  Prozess = lookup(ProzessName),
  logging(Log,format("~sSende tellmi an ~p.\n",[logHeader(self()),ProzessName])),
  Prozess ! {self(),tellmi},
  sendTellmi(Log,RProzesse).

sendPingGGT(Log,[]) ->
  logging(Log,format("~sAllen Prozesse pingGGT gesendet\n",[logHeader(self())]));
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
      logging(Log,format("~sgetsteeringval erhalten [~p,~p,~p,~p]\n",[logHeader(self()),Arbeitszeit,Termzeit,QuoteAbsolut,GGTAnz]),critical),
      loop(StaticConfig,GGTAnzahlGemeldetNeu,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {_From,getsteeringval} when Arbeitsphase == true ->
      logging(Log,format("~sgetsteeringval in Arbeitsphase erhalten, ignorieren...\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {hello,Clientname} when Arbeitsphase == false ->
      logging(Log,format("~shello erhalten von ~p\n",[logHeader(self()),Clientname])),
      GGTListeNeu = append(GGTListe,[Clientname]),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListeNeu,Arbeitsphase)
      ;
    {hello,_Clientname} when Arbeitsphase == true ->
      logging(Log,format("~shello in Arbeitsphase erhalten, ignorieren...\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {briefmi,{Clientname,CMi,CZeit}} when  Arbeitsphase == true,  AktuellKleinsterGGT == notset; Arbeitsphase == true, CMi < AktuellKleinsterGGT ->
      logging(Log,format("~sbriefmi mit Mi ~p von ~p erhalten, Zeit ~p.\n",[logHeader(self()),CMi,Clientname,now2string(CZeit)]),critical),
      loop(StaticConfig,GGTAnzahlGemeldet,CMi,GGTListe,Arbeitsphase)
      ;
    {briefmi,{_Clientname,CMi,_CZeit}} when Arbeitsphase == false; CMi >= AktuellKleinsterGGT ->
%%      logging(Log,format("~sbriefmi ignorieren...\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {From,briefterm,{Clientname,CMi,CZeit}} when Arbeitsphase == true ->
      logging(Log,format("~sbriefterm mit Mi ~p von ~p erhalten, Zeit ~p.\n",[logHeader(self()),CMi,Clientname,now2string(CZeit)])),
      if
        CMi > AktuellKleinsterGGT ->
          logging(Log,format("~sMi ist zu groß!\n",[logHeader(self())])),
          if
            HelpFlag == 1 -> From ! {sendy,AktuellKleinsterGGT};
            true -> logging(Log,format("~sBriefterm Falschmeldung\n",[logHeader(self())]))
          end;
        CMi == AktuellKleinsterGGT ->
          logging(Log,format("~sGGT entspricht dem aktuellen Mi, alles gut\n",[logHeader(self())]));
        CMi < AktuellKleinsterGGT ->
          logging(Log,format("~sBriefterm hat kleineren GGT geliefert\n",[logHeader(self())])),
          loop(StaticConfig,GGTAnzahlGemeldet,CMi,GGTListe,Arbeitsphase);
        AktuellKleinsterGGT == notset ->
          logging(Log,format("~sBriefterm vor der ersten Berechnung erhalten.\n",[logHeader(self())]))
      end,
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {_From,briefterm,{_Clientname,_CMi,_CZeit}} when Arbeitsphase == false ->
      logging(Log,format("~sbriefterm ignorieren da außerhalb der Arbeitsphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {calc,WggT} when Arbeitsphase == true->
      MiWerteListe = bestimme_mis(WggT,length(GGTListe)),
      sendMis(Log,GGTListe,MiWerteListe,setpm),
      sendStartMis(Log,WggT,GGTListe),
      logging(Log,format("~sNeue Berechnung angestoßen, Wggt: ~p.\n",[logHeader(self()),WggT]),critical),
      loop(StaticConfig,GGTAnzahlGemeldet,notset,GGTListe,Arbeitsphase)
      ;
    {calc,_WggT} when Arbeitsphase == false->
      logging(Log,format("~scalc ignorieren da außerhalb der Arbeitsphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {pongGGT,Clientname} ->
      logging(Log,format("~spongGGT erhalten von ~p\n",[logHeader(self()),Clientname])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {mi,Mi} when Arbeitsphase == true->
      logging(Log,format("~sMi erhalten: ~p\n",[logHeader(self()),Mi])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {mi,_Mi} when Arbeitsphase == false->
      logging(Log,format("~smi ignorieren da außerhalb der Arbeitsphase erhalten.\n",[logHeader(self())])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    {_From,{vote,_InitiatorName}} ->
%%      logging(Log,format("~sggt-Prozess (~p) hat vote gesendet, tue nichts...\n",[logHeader(self()),InitiatorName])),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    reset ->
      sendKill(Log,GGTListe),
      logging(Log,format("~sKoordinator reset\n",[logHeader(self())])),
      loop(StaticConfig,0,notset,[],false)
      ;
    step when Arbeitsphase == false ->
      logging(Log,format("~sstep erhalten\n",[logHeader(self())]),critical),
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
      logging(Log,format("~sKoordinator kill\n",[logHeader(self())])),
      Nameservice ! {self(),{unbind,KoordName}},
      unregister(KoordName)
      ;
    nudge ->
      sendPingGGT(Log,GGTListe),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    toggle when HelpFlag == 1 ->
      logging(Log,format("~stoggle helpFlag von true auf false\n",[logHeader(self())]),critical),
      NeuStaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,Quote,0],
      loop(NeuStaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    toggle when HelpFlag == 0 ->
      logging(Log,format("~stoggle helpFlag von false auf true\n",[logHeader(self())]),critical),
      NeuStaticConfig = [Log,Arbeitszeit,Termzeit,GGTAnz,Nameservice,KoordName,Quote,1],
      loop(NeuStaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
      ;
    Any ->
      logging(Log,format("~sHiermit können wir NICHTS anfangen: ~p\n",[logHeader(self()),Any]),critical),
      loop(StaticConfig,GGTAnzahlGemeldet,AktuellKleinsterGGT,GGTListe,Arbeitsphase)
  end.