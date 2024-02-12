---
title:                "Debug-Ausgaben drucken"
aliases:
- /de/fish-shell/printing-debug-output/
date:                  2024-01-20T17:52:36.874403-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Debug-Ausgaben zu drucken ist, als würde man unter die Motorhaube des Codes schauen. Entwickler tun dies, um zu verstehen, was während der Ausführung ihres Skripts oder Programms wirklich passiert.

## How to:
In Fish lässt sich mit `echo` und `printf` leicht Debug-Output erzeugen. Schnell und schmutzig, hier ein paar Beispiele:

```Fish Shell
# Einfach eine Variable ausgeben
set -l debug_variable "Hallo, Debugger!"
echo $debug_variable

# Formatierte Ausgabe mit printf
set -l number 42
printf "Das Geheimnis des Lebens: %d\n" $number
```

Ausgaben:
```
Hallo, Debugger!
Das Geheimnis des Lebens: 42
```

## Deep Dive
In den Anfangstagen der Programmierung wurde Debugging durch physische Indikatoren (wie etwa Lampen an Maschinen) oder Druckerausgaben realisiert. Heutzutage haben wir luxuriösere Optionen wie integrierte Debugging-Tools in Entwicklerumgebungen. Fish Shell bietet keinen eingebauten Debugger wie einige andere Sprachen, aber `echo` und `printf` sind leistungsstark für schnelles Debuggen. Du kannst Bedingungen setzen und nur bestimmte Nachrichten unter bestimmten Umständen ausgeben lassen, um deine Debugging-Nachrichten zu kontrollieren.

Alternativen zu `echo` und `printf` für komplexere Anforderungen könnten externe Logging-Tools sein oder die Nutzung einer anderen Shell mit eingebautem Debugging-Unterstützung. Bei der Implementation von Debugging-Ausgaben ist es entscheidend, diese so zu konstruieren, dass sie leicht entfernt werden können – zum Beispiel durch Umleiten in eine Datei, die ignoriert werden kann.

## See Also
Für weiterführende Informationen und speziellere Debugging-Techniken siehe:
- Die offizielle Fish Shell Dokumentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Bash vs. Fish Vergleich für Debugging: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)
