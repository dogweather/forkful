---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Erstellung einer temporären Datei ist ein Prozess, der eine temporäre Speicherstelle auf Ihrem Computer erstellt. Programmierer tun dies, um Daten kurzfristig zu speichern und zu manipulieren, ohne dauerhafte Änderungen an der zugrunde liegenden Datenquelle vorzunehmen.

## Wie:
Erstellen Sie eine temporäre Datei mit dem `mktemp`-Befehl in Bash. Hier ist ein Beispiel:
```Bash 
temp_file=$(mktemp)
echo "Hallo Welt" > $temp_file
```
Output: 
```Bash 
cat $temp_file
Hallo Welt
```
In diesem Code erstellt `mktemp` eine temporäre Datei und die Variable `temp_file` hält die Datei Weg. Dann wird "Hallo Welt” in die temporäre Datei geschrieben.

## Vertiefung:
Ursprünglich war in Unix keine eingebaute Methode zum Erstellen temporärer Dateien vorhanden. Stattdessen haben frühe Programmierer improvisiert, indem sie Dateien in einem `/tmp`-Verzeichnis erstellt haben. Heute bietet `mktemp` eine sicherere und robustere Lösung dafür. 

Alternativ zur `mktemp`-Funktion gibt es den `mkstemp`-Systemaufruf, allerdings ist dieser schwieriger zu manipulieren und eher unter fortschrittlichen Anwendern verbreitet. 

Hinsichtlich der Implementierung: eine temporäre Datei ist nicht mehr als eine normale Datei, die in einem speziellen Verzeichnis (`/tmp` oder `/var/tmp` auf den meisten Systemen) erstellt wird. Sie wird bei einem Neustart oder nach einem bestimmten Zeitraum gelöscht.

## Siehe auch:
1. [GNU mktemp man page](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
2. [Linux Documentation Project - Temp Files](https://tldp.org/LDP/abs/html/tempfiles.html)