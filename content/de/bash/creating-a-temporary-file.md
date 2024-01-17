---
title:                "Erstellung einer temporären Datei"
html_title:           "Bash: Erstellung einer temporären Datei"
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was ist das und warum? 
Das Erstellen einer temporären Datei ist ein häufiges Verfahren in der Programmierung, um vorübergehende Daten zu speichern oder zu verarbeiten. Es wird oft verwendet, wenn der Programmierer vorübergehend Daten braucht, die später gelöscht werden sollen. 

## Wie geht's weiter? 
Es gibt verschiedene Methoden, um in Bash eine temporäre Datei zu erstellen, aber eine der häufigsten ist die Verwendung des Befehls `mktemp`, der einen eindeutigen Dateinamen generiert und die Datei automatisch erstellt. 

Beispielcode: 
```Bash
tempfile=$(mktemp) 
echo "Dies ist eine temporäre Datei" > $tempfile 
cat $tempfile 
```

Erwartete Ausgabe: 
```
Dies ist eine temporäre Datei
```

## Tiefer Einblick 
Das Erstellen von temporären Dateien hat eine lange Geschichte in der Programmierung und wird seit den frühen Tagen des Unix-Betriebssystems verwendet. Es bietet eine einfache Möglichkeit, temporäre Daten zu speichern, ohne die Hauptdateien oder das System zu überladen. Eine Alternative zur Verwendung von `mktemp` besteht darin, manuell einen eindeutigen Dateinamen zu erstellen und die Datei mit dem Befehl `touch` zu erstellen. 

## Siehe auch 
Weitere Details zum `mktemp`-Befehl finden Sie in der Bash-Dokumentation unter https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html