---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Drucken von Debug-Ausgaben ist der Prozess der Anzeige von Daten während der Programmausführung. Programmierer nutzen es, um die Arbeit des Codes besser zu verstehen, Fehler aufzuspüren und die Softwarequalität zu verbessern.

## So geht's:

Im Fish Shell können wir mit der `echo` Funktion die Debug-Ausgabe drucken. So lassen sich schnell Informationen zur Laufzeit anzeigen:

```Fish Shell
function debug_output
    set -l debugging 'Debugging aktiviert'
    echo $debugging
end

debug_output
```

Dieses Programm druckt den Text "Debugging aktiviert".

## Vertiefung:

In der historischen Entwicklung liegen die Wurzeln des Debugging in der maschinennahen Programmierung, wo man die genauen Speicherinhalte während der Programmausführung direkt beobachten konnte. Im modernen Kontext stehen verschiedene Alternativen zur Verfügung, vom Einfügen von `echo` Statements bis hin zur Nutzung von dedizierten Debugging-Tools wie gdb.

Die Implementierung in Fish Shell ist recht einfach. Bei der Ausführung der `echo` Funktion sendet Fish die angegebene Zeichenkette an die Standardausgabe.

## Siehe Auch:

Weitere Ressourcen zum Thema Debugging in Fish Shell sind verfügbar auf: 
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial von GitHub](https://github.com/jorgebucaran/fish-cookbook)
- [Entwickler-Forum zu Fish Shell](https://fishshell.com/community.html)