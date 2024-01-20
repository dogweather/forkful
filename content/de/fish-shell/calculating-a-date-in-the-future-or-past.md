---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Fish Shell: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Berechnung eines zukünftigen oder vergangenen Datums ist das Ermitteln eines Datums, das bestimmte Zeiteinheiten (z.B. Tage, Wochen, Monate, Jahre) von einem bestimmten Ausgangsdatum entfernt ist. Programmierer machen das, weil es alltägliche Funktionen wie Erinnerungen, Planung von Projekten und vieles mehr ermöglicht.

## So geht's:

Mit Fish Shell ist das Berechnen eines Datums in der Zukunft oder Vergangenheit einfach möglich. Hier sind einige Beispiele:

```Fish Shell
# Einen Tag in der Zukunft berechnen
set datum (date -v+1d "+%Y-%m-%d") 
echo $datum
```
Ergebnis könnte so aussehen:

```
2022-11-30
```

```Fish Shell
# Eine Woche in der Vergangenheit berechnen
set datum (date -v-1w "+%Y-%m-%d") 
echo $datum
```
Ergebnis könnte so aussehen:

```
2022-11-16
```

## Deep Dive:

Die Fähigkeit, Datumsberechnungen durchzuführen, ist seit den frühesten Tagen der Programmierung ein wichtiges Werkzeug. Bei komplexeren Aufgaben kann es sinnvoll sein, Bibliotheken oder Tools wie GNU `date` zu nutzen, die neben einfacher Datumsaddition und -subtraktion auch erweiterte Funktionen wie die Berücksichtigung von Schaltjahren bieten. Obwohl Fish Shell nicht die umfangreichen Datumsoperatoren anderer Shells wie Bash hat, bietet sie dennoch einfachen Zugriff auf Datumsmanipulationsfunktionen durch die Integration von UNIX-Utilities wie `date`.

## Siehe auch:

- `date` Man Page (https://linux.die.net/man/1/date)
- Fish Shell Documentation (https://fishshell.com/docs/current/)
- Fish Shell GitHub (https://github.com/fish-shell/fish-shell)