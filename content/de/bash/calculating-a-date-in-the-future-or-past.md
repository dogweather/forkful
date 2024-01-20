---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Bash: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines zukünftigen oder vergangenen Datums beinhaltet die Manipulation oder Umwandlung von Datumswerten. Programmierer tun dies, um Zeitrahmen zu organisieren oder um Ereignisse zu planen.

## Wie macht man das:
Die Bash-Funktion `date` kann verwendet werden, um ein zukünftiges oder vergangenes Datum zu berechnen. Zum Beispiel:

Übersetze "+3 Tage" oder "-1 Jahr":
```Bash
# 3 Tage in die Zukunft
date -d "+3 days"

# 1 Jahr in der Vergangenheit
date -d "-1 year"
```
Die Ausgabe:
```Bash
# Zum Beispiel heute ist der 12. April
Fri Apr 15 12:34:56 CEST 2023

# Und wieder zurück zum 12. April voriges Jahr
Mon Apr 12 12:34:56 CEST 2022
```

## Tiefgreifende Infos
Historisch gesehen wurde das Berechnen von vergangenem oder zukünftigem Datum bereits von den frühesten Prozessorsystemen verwendet, und hat seinen Weg in moderne Skriptsprachen wie Bash gefunden.

Es gibt auch alternative Methoden, wie zum Beispiel die Verwendung von "GNU Coreutils" oder Shell-Skripten.

Im Zusammenhang mit der Implementierung verwendet Bash eine interne Function, um Datum und Uhrzeit zu manipulieren. Diese Funktion ermöglicht es uns, eine Vielzahl von Berechnungen mit Datumswerten vorzunehmen.

## Weiterführende Informationen
1. Bash Man Page: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
2. GNU Coreutils: [https://www.gnu.org/software/coreutils/coreutils.html](https://www.gnu.org/software/coreutils/coreutils.html)
3. Erweiterte Datumsfunktionen: [https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html]( https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)