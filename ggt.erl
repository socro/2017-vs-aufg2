-module(ggt).
-export([start/1]).

start([ ArbeitsZeit , TermZeit , Quota , GGTProzessnummer , Starternummer, Praktikumsgruppe , Teamnummer , Nameservicenode , Nameservicename , Koordinatorname ]) ->
  Nameservice = {Nameservicename, Nameservicenode},
  GGTName = create_ggt_name(Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer),
  register(GGTName,self()),
  notify_nameservice(GGTName,Nameservice),
  notify_koordinator(GGTName,Nameservice,Koordinatorname),
  % initialize Mi with 0, will get set via setpm message later
  calc_ggt_loop(GGTName,Nameservice,0)
  .

create_ggt_name(Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer) ->
  lists:concat([Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer]).

notify_nameservice(GGTName,Nameservice) ->
  Nameservice ! {self(),{rebind,GGTName,node()}},
  receive
    ok -> io:format("..rebind.done.\n")
  end.

notify_koordinator(GGTName,Nameservice, Koordinatorname) ->
  Nameservice ! {self(),{lookup,Koordinatorname}},
  receive
    not_found ->
      io:format("..~p ..not_found.\n",[Koordinatorname]);
    {pin,{Koordinatorname,Koordinatornode}} ->
      io:format("...ok: {~p,~p}.\n",[Koordinatorname,Koordinatornode]),
      Koordinator = {Koordinatorname, Koordinatornode},
      Koordinator ! {hello,GGTName}
  end.

calc_ggt_loop(GGTName,Nameservice,Mi) ->
  receive
    {setneighbors,LeftN,RightN} ->
      ;
    {setpm,MiNeu} ->
      ;
    {sendy,Y} ->
      ;
    {From,{vote,Initiator}} ->
      ;
    {voteYes,Name} ->
      ;
    {From,tellmi} ->
      From ! {mi,Mi},
      calc_ggt_loop(GGTName,Nameservice,Mi)
      ;
    {From,pingGGT} ->
      From ! {pongGGT,GGTName},
      calc_ggt_loop(GGTName,Nameservice,Mi)
end.