---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
aliases: - /de/bash/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:36.215661-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines zukünftigen oder vergangenen Datums ermöglicht es uns, Zeitintervalle zu bestimmen und automatische Erinnerungen oder Fristen in Skripten zu setzen. Programmierer nutzen diese Funktion für Features wie Ablaufüberwachungen, Terminplanungen und Zeitreisen in Datenbanken.

## So geht's:
Ein Beispiel für die Berechnung eines Datums in 7 Tagen und eines Datums vor 7 Tagen:

```Bash
# Datum in 7 Tagen
date -d "+7 days"

# Datum vor 7 Tagen
date -d "-7 days"
```

Mögliche Ausgabe:

```Bash
Do 13 Apr 2023 12:34:56 CEST
Do 30 Mär 2023 12:34:56 CEST
```

## Tiefergehende Einblicke
Der `date` Befehl in Bash ist vielseitig. Früher mussten solche Berechnungen manuell oder mit komplizierter Software ausgeführt werden. Mittlerweile ist es ein Kinderspiel. Alternativen wie `dateutils` bieten noch mehr Funktionen an, sind aber nicht standardmäßig vorhanden. Die Implementierungsdetails beruhen auf der Verwendung von GNU Coreutils `date`. Eingebaute Methoden zum Hinzufügen oder Subtrahieren von Datumsangaben beachten automatisch Schaltjahre, Monatslängen und Zeitzone.

## Siehe auch
- GNU Coreutils Manual für `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Mehr über `dateutils`: http://www.fresse.org/dateutils/
- Bash Scripting Guide: http://www.tldp.org/LDP/abs/html/
