---
date: 2024-01-26 03:47:35.372668-07:00
description: "Wie geht das: Bash kommt nicht mit einem eingebauten Debugger wie einige\
  \ andere Sprachen, aber Sie k\xF6nnen eingebaute Befehle wie `set -x` verwenden,\
  \ um\u2026"
lastmod: '2024-03-13T22:44:54.064162-06:00'
model: gpt-4-0125-preview
summary: "Bash kommt nicht mit einem eingebauten Debugger wie einige andere Sprachen,\
  \ aber Sie k\xF6nnen eingebaute Befehle wie `set -x` verwenden, um nachzuvollziehen,\
  \ was passiert."
title: Einsatz eines Debuggers
weight: 35
---

## Wie geht das:
Bash kommt nicht mit einem eingebauten Debugger wie einige andere Sprachen, aber Sie können eingebaute Befehle wie `set -x` verwenden, um nachzuvollziehen, was passiert. Oder, für ein Upgrade, gibt es `bashdb`, einen richtigen Debugger, um durch Ihren Code zu schreiten. Hier ist ein Einblick:

```Bash
# Debugging mit set -x
set -x
echo "Starte Debugging"
my_var="Hallo, Debugging Welt!"
echo $my_var
set +x

# Verwendung von bashdb
# Installieren Sie bashdb mit Ihrem Paketmanager, z. B. apt, yum, brew.
# Debuggen eines Skripts namens my_script.sh:
bashdb my_script.sh
```

Ausgabe beim Ausführen mit `set -x`:
```Bash
+ echo 'Starte Debugging'
Starte Debugging
+ my_var='Hallo, Debugging Welt!'
+ echo 'Hallo, Debugging Welt!'
Hallo, Debugging Welt!
+ set +x
```

## Tiefergehend
Historisch gesehen bedeutete das Debuggen von Bash-Skripten, Ihren Code mit `echo`-Anweisungen zu übersäen. Aber dann kam `set -x`, das uns einen Einblick in die Laufzeitausführung gab, ohne manuelle Ausdrucke. Und für diejenigen, die mehr Kontrolle wünschen, tauchte der Debugger `bashdb` auf, inspiriert vom gdb Debugger für C/C++.

Was Alternativen betrifft, über die `set`-Befehle (`-x`, `-v`, `-e`) hinaus, gehören zu den anderen Optionen die Umleitung der Ausgabe in eine Datei zur Analyse oder die Verwendung externer Tools wie ShellCheck für statische Analysen.

Implementierungstechnisch ist `set -x` einfach; es ist eine native Bash-Option, die Befehle und ihre Argumente druckt, während sie ausgeführt werden. `bashdb` hingegen ermöglicht das Durchschreiten von Code, das Setzen von Haltepunkten und das Auswerten von Ausdrücken – Dinge, die Ihnen eine Kampfchance gegen schwer fassbare Fehler geben.

## Siehe auch
- Bash Debugger Projekt: http://bashdb.sourceforge.net/
- "Pro Bash Programming" von Chris Johnson und Jayant Varma für fortgeschrittenes Scripting.
- ShellCheck für statische Analyse: https://www.shellcheck.net/
