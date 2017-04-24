-module(ggt).
-import(helper,[logHeader/1,notify_nameservice/3,lookup/1,logging/3]).
-import(werkzeug,[logging/2]).
-import(io_lib,[format/2]).
-export([start/1]).

start([ ArbeitsZeit , TermZeit , Quota , GGTProzessnummer , Starternummer, Praktikumsgruppe , Teamnummer , Nameservicenode , Nameservicename , Koordinatorname ]) ->
  TermZeitHalbe = erlang:round(TermZeit / 2),
  Nameservice = {Nameservicename, Nameservicenode},
  GGTName = create_ggt_name(Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer),
  Log = format("logs/ggts/~p.log",[GGTName]),
  register(GGTName,self()),
  net_adm:ping(Nameservicenode),
  notify_nameservice(Log,GGTName,Nameservice),
  timer:sleep(500),
  notify_koordinator(Log,GGTName,Koordinatorname),
  % initialize
  %   Mi = 0, will get set via setpm message later
  %   Timer as atom, -> has no effect, will be set later
  %   neightbours as atoms, -> has no effect, will be set later
  %   AnzahlVoteYesErhalten = 0
  %   HalbeTermZeitVergangen = false
  StaticConfig = [Log,GGTName,Nameservice,Koordinatorname,ArbeitsZeit,TermZeitHalbe,Quota],
  calc_ggt_loop(StaticConfig,0,leftneighbor,rightneighbor,timer,0,false)
  .

create_ggt_name(Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer) ->
  %format("~p~p~p~p",[Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer]).
  list_to_atom(lists:concat([Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer])).

notify_koordinator(Log,GGTName,Koordinatorname) ->
  Koordinator = lookup(Koordinatorname),
  Koordinator ! {hello,GGTName}.

calc_mi(Mi,Y,ArbeitsZeit) ->
  timer:sleep(ArbeitsZeit*1000),
  ((Mi-1) rem Y)+1.

send_mi_neighbors(Log,Mi,LeftNeighborName,RightNeighborName) ->
  LeftNeighbor = lookup(LeftNeighborName),
  LeftNeighbor ! {sendy,Mi},
  RightNeighbor = lookup(RightNeighborName),
  RightNeighbor ! {sendy,Mi}.

request_votes(GGTName,Nameservice) -> Nameservice ! {self(),{multicast,vote,GGTName}}.

send_ggt(Log,GGTName,Koordinatorname,Mi,MsgType) ->
  Koordinator = lookup(Koordinatorname),
  if
    MsgType == briefterm -> Koordinator ! {self(),briefterm,{GGTName,Mi,erlang:timestamp()}};
    MsgType == briefmi -> Koordinator ! {briefmi,{GGTName,Mi,erlang:timestamp()}}
  end.

calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten,HalbeTermZeitVergangen) ->
  [Log,GGTName,Nameservice,Koordinatorname,ArbeitsZeit,TermZeitHalbe,Quota] = StaticConfig,
  receive
    % public messages
    {setneighbors,NewLeftNeighborName,NewRightNeighborName} ->
      calc_ggt_loop(StaticConfig,Mi,NewLeftNeighborName,NewRightNeighborName,Timer,AnzahlVoteYesErhalten,HalbeTermZeitVergangen)
      ;
    {setpm,NewMi} ->
      NewTimer = werkzeug:reset_timer(Timer,TermZeitHalbe,{terminateAfterTimeout}),
      logging(Log,format("~sggt-Prozess (~p) soll eine neue Berechnung durchfuehren (setpm ~p erhalten)\n",[logHeader(self()),GGTName,NewMi])),
      calc_ggt_loop(StaticConfig,NewMi,LeftNeighborName,RightNeighborName,NewTimer,0,false)
      ;
    {sendy,Y} when Y < Mi ->
      NewTimer = werkzeug:reset_timer(Timer,TermZeitHalbe,{terminateAfterTimeout}),
      logging(Log,format("~sY (~p) erhalten, ist < Mi (~p), berechne neues Mi...\n",[logHeader(self()),Y,Mi])),
      NewMi = calc_mi(Mi,Y,ArbeitsZeit),
      send_mi_neighbors(Log,NewMi,LeftNeighborName,RightNeighborName),
      send_ggt(Log,GGTName,Koordinatorname,NewMi,briefmi),
      calc_ggt_loop(StaticConfig,NewMi,LeftNeighborName,RightNeighborName,NewTimer,0,false)
      ;
    {sendy,Y} when Y >= Mi ->
      NewTimer = werkzeug:reset_timer(Timer,TermZeitHalbe,{terminateAfterTimeout}),
      logging(Log,format("~sY (~p) erhalten, ist >= Mi (~p), tue nichts.\n",[logHeader(self()),Y,Mi])),
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,NewTimer,0,false)
      ;
    {From,{vote,Initiator}} when HalbeTermZeitVergangen == true->
      logging(Log,format("~sggt-Prozess (~p) meldet: Terminierungsanfrage erhalten, antworte sendYes...\n",[logHeader(self()),GGTName])),
      From ! {voteYes,GGTName},
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten,HalbeTermZeitVergangen)
      ;
    {From,{vote,InitiatorName}} when HalbeTermZeitVergangen == false->
      logging(Log,format("~sggt-Prozess (~p) meldet: Terminierungsanfrage erhalten, ignorieren...\n",[logHeader(self()),GGTName])),
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten,HalbeTermZeitVergangen)
      ;
    {voteYes,Name} when AnzahlVoteYesErhalten+1 == Quota ->
      logging(Log,format("~sggt-Prozess (~p) meldet: voteYes erhalten, Quota erreicht, sende ggt...\n",[logHeader(self()),GGTName])),
      send_ggt(Log,GGTName,Koordinatorname,Mi,briefterm),
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten+1,HalbeTermZeitVergangen)
      ;
    {voteYes,Name} when AnzahlVoteYesErhalten+1 < Quota; AnzahlVoteYesErhalten+1 > Quota ->
      logging(Log,format("~sggt-Prozess (~p) meldet: voteYes erhalten, Anzahl Votes entsprechen nicht dem Quota...\n",[logHeader(self()),GGTName])),
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten+1,HalbeTermZeitVergangen)
      ;
    {From,tellmi} ->
      From ! {mi,Mi},
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten,HalbeTermZeitVergangen)
      ;
    {From,pingGGT} ->
      From ! {pongGGT,GGTName},
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,AnzahlVoteYesErhalten,HalbeTermZeitVergangen)
      ;
    kill ->
      logging(Log,format("~sggt-Prozess (~p) meldet: kill erhalten.\n",[logHeader(self()),GGTName]))
      ;
    % internal messages
    {terminateAfterTimeout} when HalbeTermZeitVergangen == true->
      logging(Log,format("~sggt-Prozess (~p) meldet: Terminierungszeit abgelaufen, starte Terminierungsanfrage...\n",[logHeader(self()),GGTName])),
      request_votes(GGTName,Nameservice),
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,Timer,0,true)
      ;
    {terminateAfterTimeout} when HalbeTermZeitVergangen == false->
      logging(Log,format("~sggt-Prozess (~p) meldet: halbe Terminierungszeit vergangen...\n",[logHeader(self()),GGTName])),
      NewTimer = werkzeug:reset_timer(Timer,TermZeitHalbe,{terminateAfterTimeout}),
      calc_ggt_loop(StaticConfig,Mi,LeftNeighborName,RightNeighborName,NewTimer,AnzahlVoteYesErhalten,true)
end.