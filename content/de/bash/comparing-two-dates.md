---
title:                "Vergleich von zwei Daten"
html_title:           "Bash: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Vergleichung von zwei Daten beschäftigen? Nun, es könnte verschiedene Gründe geben. Vielleicht arbeitet man an einem Projekt, bei dem man Daten aus verschiedenen Quellen zusammenführen muss oder man möchte einfach nur überprüfen, ob zwei Ereignisse an einem bestimmten Tag stattfanden.

## Wie

Um zwei Daten in Bash zu vergleichen, gibt es verschiedene Wege. Hier sind zwei Beispiele:

```Bash
# Beispiel 1: Vergleich von zwei Daten mit dem Vergleichsoperator "-eq"
if [[ $date1 -eq $date2 ]]; then
  echo "Die Daten sind gleich."
else
  echo "Die Daten sind unterschiedlich."
fi
```

```Bash
# Beispiel 2: Vergleich von zwei Daten mit dem "date"-Befehl
if [[ $(date -d "$date1" +%s) -eq $(date -d "$date2" +%s) ]]; then
  echo "Die Daten sind gleich."
else
  echo "Die Daten sind unterschiedlich."
fi
```

Die erste Methode verwendet den Bash-Vergleichsoperator "-eq", der für die Vergleichung numerischer Werte verwendet wird. Die zweite Methode nutzt den "date"-Befehl, um beide Daten in Sekunden seit dem 01.01.1970 zu konvertieren und dann miteinander zu vergleichen.

### Ausgabe:

```Bash
$ date1="12/13/2020"
$ date2="12/13/2020"
$ ./script.sh
Die Daten sind gleich.
```

```Bash
$ date1="12/13/2020"
$ date2="12/14/2020"
$ ./script.sh
Die Daten sind unterschiedlich.
```

## Deep Dive

Der Vergleich von Daten in Bash kann auch etwas komplexer sein. Hier sind einige Dinge, die man beachten sollte:

- Verwenden von führenden Nullen: Wenn beide Daten im selben Format sind, ist es wichtig, dass beide Daten führende Nullen aufweisen. Ansonsten könnte es zu fehlerhaften Ergebnissen kommen.

- Berücksichtigung von Zeitzonen: Wenn man mit Daten arbeitet, die an verschiedenen Orten in verschiedenen Zeitzonen erfasst wurden, sollte man diese Informationen berücksichtigen oder die Daten auf eine gemeinsame Zeitzone konvertieren, bevor man sie vergleicht.

- Verwendung von Epoch Time: Die Konvertierung der Daten in Sekunden seit dem 01.01.1970 (Epoch Time) kann hilfreich sein, besonders wenn man mit größeren Datenmengen arbeitet.

Es gibt auch andere mögliche Probleme und Szenarien beim Vergleich von Daten, die man bedenken sollte, je nach den Anforderungen des Projekts.

## Siehe auch

Hier sind einige Links zu weiterführenden Informationen über die Vergleichung von Daten in Bash:

- [Bash-Vergleichsoperatoren](https://wiki.ubuntuusers.de/Bash/Vergleichsoperatoren/)
- [Der "date"-Befehl in Bash](https://wiki.ubuntuusers.de/date/)
- [Epoch Time](https://de.wikipedia.org/wiki/Unixzeit)