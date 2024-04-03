---
date: 2024-01-20 17:28:36.215661-07:00
description: "Das Berechnen eines zuk\xFCnftigen oder vergangenen Datums erm\xF6glicht\
  \ es uns, Zeitintervalle zu bestimmen und automatische Erinnerungen oder Fristen\
  \ in\u2026"
lastmod: '2024-03-13T22:44:54.072640-06:00'
model: gpt-4-1106-preview
summary: "Das Berechnen eines zuk\xFCnftigen oder vergangenen Datums erm\xF6glicht\
  \ es uns, Zeitintervalle zu bestimmen und automatische Erinnerungen oder Fristen\
  \ in Skripten zu setzen."
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

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
