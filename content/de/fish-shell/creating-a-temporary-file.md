---
title:                "Erstellung einer temporären Datei"
date:                  2024-01-20T17:40:11.014365-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Erstellen einer temporären Datei ermöglicht es, kurzlebige Daten zu speichern, ohne sich um manuelles Aufräumen kümmern zu müssen. Programmierer nutzen dies für Daten, die nur während der Laufzeit eines Programms benötigt werden und danach irrelevant sind.

## How to: (Wie geht das:)
In Fish Shell kannst du schnell und sicher eine temporäre Datei erzeugen. Hier sind praktische Beispiele:

```Fish Shell
# Temporäre Datei erstellen und Pfad speichern
set temp_file (mktemp)

# Arbeit mit der temporären Datei
echo "Testinhalt" > $temp_file

# Inhalt der temporären Datei anzeigen
cat $temp_file
```
Beispiel-Ausgabe:
```
Testinhalt
```

Nach der Verwendung wird die temporäre Datei automatisch vom System gelöscht, wenn das Terminal geschlossen wird.

## Deep Dive (Tiefer Eintauchen)
Das Konzept temporärer Dateien gibt es schon seit den frühen Tagen der Unix-Systeme. Sie sind wichtig, um Kollisionen bei Dateinamen zu vermeiden und die Sicherheit zu verbessern, da temp. Dateien oft in einem speziellen Verzeichnis mit besonderen Berechtigungen liegen.

Unter Fish Shell (und Unix-artigen Systemen) wird `mktemp` verwendet, ein Befehl, der sicherstellt, dass jede erzeugte Datei einzigartig ist. Im Gegensatz zu einigen Skriptsprachen, die eigene Funktionen zur Erstellung von temporären Dateien mitbringen, nutzt Fish Shell externe Programme.

Fish Shell bietet eine simplere Syntax und verbesserte Eigenschaften für den täglichen Gebrauch im Vergleich zur traditionell strengeren Bash-Syntax. Sie ist besonders nützlich im interaktiven Gebrauch und für kleine bis mittlere Skripte.

Die Alternative zum manuellen Erstellen einer temporären Datei wäre die Verwendung des `/tmp` Verzeichnisses, das jedoch das Risiko birgt, Dateikonflikte manuell verwalten zu müssen.

## See Also (Weitere Quellen)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix 'mktemp' Man Page](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [Fish Shell Tutorial für Anfänger](https://wiki.ubuntuusers.de/Fish/)
- [Advanced Fish Scripting Guide](https://fishshell.com/docs/current/tutorial.html#tut_scripts)
