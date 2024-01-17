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

# Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit bezieht sich auf die Manipulation von Datumsangaben, um ein bestimmtes Datum in der Zukunft oder Vergangenheit zu erhalten. Programmierer verwenden dies häufig, um automatisierte Aufgaben wie die Planung von Ereignissen oder die Überprüfung von Ablaufdaten durchzuführen.

# So geht's:
Alle unten stehenden Beispiele können in der Bash-Ausführungsumgebung eingegeben werden. Beachten Sie, dass das aktuelle Datum für alle Beispiele der 24. Oktober 2021 ist.

#### Berechnen eines Datums in der Zukunft:
```
Bash
date -d "next week" +%Y-%m-%d
2021-10-31
```

#### Berechnen eines Datums in der Vergangenheit:
```
Bash
date -d "2 days ago" +%A
Friday
```

#### Berechnen eines Datums mit einem bestimmten Format:
```
Bash
date -d "next month" +"Heute ist %B %d, %Y"
Heute ist November 24, 2021
```

# Tiefenschärfe:
- Das Berechnen von Datumsangaben in der Bash-Umgebung ist eine Funktion des Befehls "date". Dieser Befehl kann auch verwendet werden, um die aktuelle Zeit und andere Zeitformate anzuzeigen.
- Eine alternative Möglichkeit, Datumsangaben in der Zukunft oder Vergangenheit zu berechnen, besteht darin, die Programmbibliothek "datetime" in Python zu verwenden.
- Die Implementation erfolgt durch Verwendung von Unix-Zeitstempeln und mathematischen Operationen, um zu dem gewünschten Datum zu gelangen.

# Siehe auch:
- Offizielle Dokumentation des Befehls "date": https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Einführung in die Verwendung von Unix-Zeitstempeln: https://www.epochconverter.com/