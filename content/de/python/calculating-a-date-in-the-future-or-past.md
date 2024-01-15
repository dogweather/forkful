---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Python: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Daten kann in vielen Situationen nützlich sein, z.B. um an einem bestimmten Datum eine Erinnerung zu setzen oder um Terminpläne zu erstellen. Mit Python können wir einfach und genau solche Datumsberechnungen durchführen.

## Wie geht's

```Python
from datetime import datetime, timedelta
# importiere das datetime-Modul, um auf Datumsfunktionen zuzugreifen

# Berechnung eines zukünftigen Datums
heute = datetime.now()  # heute als datetime-Objekt
zukunft = heute + timedelta(days=5)  # 5 Tage hinzufügen
print(zukunft)  # gibt das Ergebnis als Datum im Format "YYYY-MM-DD HH:MM:SS" aus

# Berechnung eines vergangenen Datums
nachher = datetime(2020, 1, 1)  # ein bestimmtes Datum als datetime-Objekt
vorher = nachher - timedelta(weeks=2)  # 2 Wochen abziehen
print(vorher)  # gibt das Ergebnis als Datum im Format "YYYY-MM-DD" aus
```

Der Code oben zeigt, wie wir mit Hilfe des datetime-Moduls und der timedelta-Funktion ein zukünftiges oder vergangenes Datum berechnen können. Es ist wichtig zu beachten, dass die timedelta-Funktion mit verschiedenen Einheiten wie Tagen, Wochen, Monaten, etc. arbeiten kann.

## Tiefere Einblicke

Das datetime-Modul bietet noch weitere nützliche Funktionen, wie z.B. das Formatieren von Datum und Uhrzeit oder die Umrechnung von Datum in andere Zeitzonen. Außerdem können wir mit der timedelta-Funktion nicht nur Datumsberechnungen durchführen, sondern auch Zeitdifferenzen zwischen zwei Daten bestimmen.

## Siehe auch

- [Tutorial: Date and Time in Python](https://realpython.com/python-datetime/)
- [Official Python documentation on datetime module](https://docs.python.org/3/library/datetime.html)
- [Mastering Python datetime](https://stackabuse.com/mastering-datetime-in-python/)