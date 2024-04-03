---
date: 2024-01-20 17:30:40.412013-07:00
description: 'How to: Berechne ein Datum in der Zukunft.'
lastmod: '2024-03-13T22:44:54.322432-06:00'
model: gpt-4-1106-preview
summary: Berechne ein Datum in der Zukunft.
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

## How to:
Berechne ein Datum in der Zukunft:

```Fish Shell
set -l zukunft (date -d '+1 week' "+%Y-%m-%d")
echo $zukunft
```

Ausgabe könnte sein: `2023-04-17` (abhängig vom heutigen Datum)

Ein Datum in der Vergangenheit ermitteln:

```Fish Shell
set -l vergangenheit (date -d '-1 month' "+%Y-%m-%d")
echo $vergangenheit
```

Ausgabe könnte sein: `2023-03-10` (abhängig vom heutigen Datum)

## Deep Dive
Früher benutzten Leute physische Kalender und mussten manuell nachzählen. Programmiersprachen bieten heute Funktionen, um diese Berechnungen automatisch durchzuführen. Neben `date` in Shell-Skripten gibt es spezialisierte Bibliotheken in Sprachen wie Python (`datetime`) oder JavaScript (`Date`), die flexiblere und mächtigere Möglichkeiten zur Datumshandhabung bieten. In Fish Shell verlassen wir uns auf externe Tools wie `date`, da es keine eingebaute Datum-Funktionalität gibt. Beachten sollte man auch die Systemabhängigkeit von `date` – GNU `date` auf Linux unterscheidet sich von BSD `date` auf MacOS.

## See Also
- [GNU Coreutils Documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) für `date`
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) für allgemeine Fish Shell Verwendung
- [Python datetime Documentation](https://docs.python.org/3/library/datetime.html) für ein Beispiel in einer anderen Programmiersprache
