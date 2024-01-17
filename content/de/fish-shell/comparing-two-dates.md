---
title:                "Vergleich von zwei Daten"
html_title:           "Fish Shell: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist ein grundlegender Bestandteil der Programmierung. Es bezieht sich auf das Ermitteln von Unterschieden oder Ähnlichkeiten zwischen zwei Datumsangaben, um Entscheidungen in einem Programm zu treffen. Programmierer nutzen diese Funktion, um zum Beispiel festzustellen, ob ein bestimmtes Datum in der Vergangenheit oder Zukunft liegt oder um verschiedene Datumsformate zu vergleichen.

## So funktioniert's:
Um in Fish Shell zwei Daten zu vergleichen, können wir den Befehl `date` verwenden, der standardmäßig auf den aktuellen Zeitpunkt zurückgreift. Wir können dann die Vergleichsoperatoren `-gt` (größer als), `-lt` (kleiner als), `-eq` (gleich) oder `-ne` (ungleich) verwenden, um das gewünschte Ergebnis zu erhalten. Hier sind einige Beispiele:

```
# Prüfen, ob das aktuelle Datum vor dem 1. Januar 2020 liegt
if date -lt 2020-01-01
    echo "Wir sind in der Vergangenheit!"
end

# Überprüfen, ob das Datum in der Variable `date1` vor dem Datum in der Variable `date2` liegt
if test "$date1" -lt "$date2"
    echo "date1 ist früher als date2"
end
```

## Tiefere Einblicke:
Das Vergleichen von Daten hat in der Geschichte der Programmierung immer eine wichtige Rolle gespielt. Es ermöglichte es den Programmierern, Zeitmanipulationen durchzuführen und komplexe Entscheidungen basierend auf dem aktuellen Datum zu treffen. Es gibt auch alternative Ansätze wie den Vergleich mit Unix-Timestamps anstelle von Datumsangaben oder die Verwendung von externen Bibliotheken für die genaue Verarbeitung von Datumsformaten. Fish Shell verwendet die Funktion `date`, um die aktuellen Zeitinformationen zu erhalten, und nutzt die Vergleichsoperatoren, um die Art und Weise zu steuern, wie diese Informationen verglichen werden.

## Siehe auch:
- Offizielle Fish Shell-Dokumentation für den Befehl `date`: https://fishshell.com/docs/current/commands.html#date
- Unix-Timestamps erläutert: https://de.wikipedia.org/wiki/Unixzeit
- Vergleichsoperatoren in der Bash-Shell: https://linuxize.com/post/bash-comparison-operators/