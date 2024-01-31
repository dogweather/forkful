---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Schreiben auf Standardfehler (stderr) nutzt man für Fehlermeldungen und Diagnostik. Es ermöglicht, normale Ausgaben von Fehlermeldungen zu trennen, sodass sie leichter protokolliert und untersucht werden können.

## How to:
```Fish Shell
# Fehlermeldung auf stderr ausgeben
echo "Fehler: Datei nicht gefunden!" >&2

# Überprüfen mit: die Meldung wird auf dem Bildschirm angezeigt
```
```Fish Shell
# Normale Ausgabe und stderr auf unterschiedliche Dateien umleiten
echo "Alles klar" > output.txt
echo "Etwas ging schief" >&2 1>error.txt

# output.txt enthält "Alles klar"
# error.txt enthält "Etwas ging schief"
```

## Deep Dive
Stderr (Standardfehler) wurde zusammen mit stdout (Standardausgabe) als Teil der Unix-Philosophie eingeführt. Alternativen zum Schreiben auf stderr in Fisch sind unter anderem die Nutzung von logger-Tools oder speziellen Logging-Frameworks. In Fish wird stderr mit `>&2` adressiert, wobei `2` der Dateideskriptor für Standardfehler ist.

## See Also
- [Fish Scripting Manual](https://fishshell.com/docs/current/index.html)
- [Unix Standard Streams Wikipedia](https://de.wikipedia.org/wiki/Standard-Datenstr%C3%B6me)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
