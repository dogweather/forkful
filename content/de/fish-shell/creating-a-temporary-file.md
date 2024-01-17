---
title:                "Erstellen einer temporären Datei"
html_title:           "Fish Shell: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei ist eine gängige Aufgabe für Programmierer, die es ihnen ermöglicht, vorübergehende Daten oder Ergebnisse zu speichern, bevor sie in eine dauerhafte Datei geschrieben werden. Dies kann nützlich sein, um Platz zu sparen oder um sicherzustellen, dass die endgültige Datei nicht versehentlich überschrieben wird.

## Wie?

Fish Shell bietet eine eingebaute Funktion `mktemp` für das Erstellen von temporären Dateien. Um eine temporäre Datei zu erstellen, können Sie einfach `mktemp` gefolgt von einem Dateinamen mit Platzhaltern wie `XXXXXX` aufrufen. Zum Beispiel:
```
fish shell> mktemp temp_file_XXXXXX
```
Die Platzhalter sorgen dafür, dass es sich um eine eindeutige Datei handelt. Die Ausgabe wird dann ein Pfad zur neu erstellten temporären Datei sein, zum Beispiel:
```
/var/folders/12/3456/temp_file_K3FbM5
```
Sie können die erstellte Datei auch direkt öffnen, indem Sie den Befehl `mktemp` mit `open` kombinieren:
```
fish shell> open (mktemp temp_file_XXXXXX)
```
Dies öffnet die temporäre Datei in der Standardanwendung für das entsprechende Dateiformat.

## Tiefeneintauchen

Das Erstellen von temporären Dateien ist eine gängige Methode, die bereits seit den Anfängen der Programmierung verwendet wird. Es gibt jedoch auch alternative Methoden, wie zum Beispiel das Speichern von temporären Daten im Arbeitsspeicher oder das Verwenden von Verzeichnissen als temporäre Dateien.

Die `mktemp` Funktion in Fish Shell verwendet die `mktemp`-Befehle des Betriebssystems und ist daher plattformunabhängig. Sie können auch ein eigenes Skript erstellen, um eine temporäre Datei zu erstellen, indem Sie die Dateierweiterung `.tmp` verwenden. Diese wird von vielen Betriebssystemen als temporäre Datei erkannt und automatisch gelöscht, wenn das Programm geschlossen wird.

## Siehe auch

Weitere Informationen über die `mktemp` Funktion und das Erstellen von temporären Dateien finden Sie in der offiziellen Dokumentation von Fish Shell: https://fishshell.com/docs/current/cmds/mktemp.html