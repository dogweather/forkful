---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:14:23.718036-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist die Methode, mit der du das heutige Datum in deine Shell-Skripte holst. Programmierer nutzen es für Logs, Zeitstempel und zeitabhängige Funktionen.

## So Geht's:
```Fish Shell
set datum (date '+%Y-%m-%d')
echo $datum
```
Ausgabe:
```
2023-04-02
```

## Tiefergehend:
Das `date`-Kommando existiert schon seit den Anfängen der Unix-Ära und ist aus der täglichen Programmierung kaum wegzudenken. In Fish Shell wird es oft in verketteten Befehlen oder Shell-Skripten verwendet, um dynamisch mit dem Datum zu arbeiten. Alternative Wege das Datum abzurufen gibt es durch diverse externe Tools oder Programmiersprachen, doch `date` bleibt wegen seiner Einfachheit und universellen Verfügbarkeit oft die erste Wahl. Die Formatierung des Datums erfolgt über Standard-Zeichenketten, wobei `%Y-%m-%d` ein verbreitetes ISO 8601-Format darstellt.

## Siehe auch:
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils 'date'](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [ISO 8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)