---
title:    "Bash: Vergleich von zwei Daten"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten kann ein wichtiger Teil der Bash-Programmierung sein, da es uns ermöglicht, Bedingungen basierend auf bestimmten Zeitpunkten zu definieren. Zum Beispiel könnte man einen Skript haben, der bestimmtes Verhalten ausführt, basierend darauf, ob das aktuelle Datum vor oder nach einem festgelegten Datum liegt.

# Wie geht man vor?

Um zwei Daten in Bash zu vergleichen, können wir den Befehl `[[ $datum1 -operatoren $datum2 ]]` verwenden. Dabei tauschen wir "datum1" und "datum2" mit den zu vergleichenden Daten aus und nutzen einen passenden Vergleichsoperator, wie zum Beispiel "gt" (größer als) oder "lt" (kleiner als). Im Folgenden sind einige Code-Beispiele mit Erklärungen und Ausgaben.

```Bash
#!/bin/bash

# Datum-Variable erstellen
datum_1="2021-01-01"
# Aktuelles Datum erhalten
datum_2=$(date +%F)

# Vergleich: ist $datum_2 größer als $datum_1?
if [[ $datum_2 -gt $datum_1 ]]; then
    echo "Heute ist $datum_2, das ist später als $datum_1."
fi

# Vergleich: sind $datum_1 und $datum_2 gleich?
if [[ $datum_1 -eq $datum_2 ]]; then
    echo "Heute ist $datum_1, das ist das gleiche Datum wie $datum_2."
fi
```

Das obige Skript gibt die folgende Ausgabe aus:

```
Heute ist 2021-07-13, das ist später als 2021-01-01.
```

# Tiefere Einblicke

Beim Vergleichen von Daten in Bash gibt es einige wichtige Dinge zu beachten. Zum einen müssen die Datumsformate genau übereinstimmen, sonst wird der Vergleich fehlschlagen. Zum Beispiel werden "01/01/2021" und "2021-01-01" nicht als gleich betrachtet.

Zum anderen gibt es verschiedene Vergleichsoperatoren, die je nach Anwendungsfall verwendet werden können. Eine vollständige Liste der unterstützten Operatoren findet man in der Bash-Dokumentation.

Gerade bei der Verwendung von Datumsvariationen wie Monaten oder Jahren kann es zu unerwarteten Ergebnissen kommen. Es ist daher wichtig, die Datumsformate genau zu definieren und bei Bedarf mit den erhältlichen Bash-Funktionen (wie zum Beispiel `date`) entsprechend zu bearbeiten.

# Siehe auch

Hier sind einige nützliche Links, um deine Bash-Programmierkenntnisse zu vertiefen:

- [Bash-Referenzhandbuch] (https://www.gnu.org/software/bash/manual/bash.html)
- [Bash-Skripting-Tutorial] (https://wiki.bash-hackers.org/scripting/tutoriallist)
- [Bash-Aufgaben zum Üben] (https://wolfermann.org/bash-uebungsaufgaben)