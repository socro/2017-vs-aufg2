------------------------
Compilieren der Dateien:
------------------------
Zu dem Paket gehören die Dateien
koordinator.erl; ggt.erl; starter.erl; helper.erl; werkzeug.erl ;
nameservice.beam

sowie:
Readme.txt; koordinator.cfg; ggt.cfg

erl -name adminNode -setcookie moin
1> c(koordinator),c(ggt),c(helper),c(werkzeug),c(starter).

--------------------
Hinweis zum Starten:
--------------------
Beim Starten der Nodes ist darauf zu achten, den Namen der Node entsprechend den cfg-Dateien zu wählen

--------------------
Starten der Nodes und Dienste
--------------------
erl -name nameserviceNode -setcookie moin
1> nameservice:start().

erl -name koordinatorNode -setcookie moin
1> koordinator:start().

erl -name starterNode1 -setcookie moin
1> starter:start(1).

--------------------
Starte einer Berechnung:
dazu in der Erlang Shell auf dem adminNode (siehe oben, Compilieren)...
--------------------
1> {koordinator,'koordinatorNode@localhost.localdomain'} ! step.
1> {koordinator,'koordinatorNode@localhost.localdomain'} ! {calc,13}.

-------------
Runterfahren:
dazu in der Erlang Shell auf dem adminNode...
-------------
1> {koordinator,'koordinatorNode@localhost.localdomain'} ! kill.

anschließend müssen die Nodes jeweils manuell bendet werden
1> Ctrl/Strg Shift G
-->q
