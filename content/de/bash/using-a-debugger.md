---
title:                "Einsatz eines Debuggers"
aliases:
- de/bash/using-a-debugger.md
date:                  2024-01-26T03:47:35.372668-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Verwendung eines Debuggers in Bash bedeutet, Tools zu nutzen, um Probleme in Ihren Skripten zu testen und zu finden, wie das Einfangen von Fehlern, die Ihren Code zum Absturz bringen oder ihn heimlich fehlverhalten lassen. Programmierer tun dies, weil es weitaus klüger ist, Fehler zu erkennen, bevor sie in einer Live-Umgebung Chaos anrichten.

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
