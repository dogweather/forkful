---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Python: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Artikel: Datenberechnung in der Zukunft und Vergangenheit in Python

## Was & Warum?
Die Berechnung eines zukünftigen oder vergangenen Datums bezieht sich auf die Manipulation von Datumsangaben basierend auf Tagen, Wochen oder Monaten. Programmierer führen dies aus, um Aufgaben wie Fristenpflege, Ereignis-Countdowns oder Zeitspannenrechnung effizient zu handhaben.

## Anleitung:
In Python verwenden wir die `datetime`- und `date`-Bibliotheken, um dies zu erreichen. Hier sind einige einfache Beispiele:

Fügen Sie 100 Tage zu einem Datum hinzu:

```python
from datetime import datetime, timedelta
heute = datetime.today()
in_100_tagen = heute + timedelta(days=100)
print(in_100_tagen)
```
Subtrahieren Sie 2 Wochen von einem Datum:

```python
from datetime import datetime, timedelta
heute = datetime.today()
vor_2_wochen = heute - timedelta(weeks=2)
print(vor_2_wochen)
```

Laufen diese Codes aus, erhalten Sie das Datum von heute plus 100 Tagen bzw. minus 2 Wochen.

## Vertiefung:
Die `datetime`- und `timedelta`-Bibliotheken haben ihre Wurzeln in der Unix-Ära. Sie basieren auf dem Konzept der Unix-Zeit, bei der die Zeit als Anzahl der Sekunden seit dem 1. Januar 1970 gemessen wird.

Alternativ kann man auch externe Bibliotheken wie Dateutil, Arrow und Pendulum verwenden, die erweitere Funktionen für die Datums- und Zeitmanipulation bieten.

Zur Berechnung des zukünftigen oder vergangenen Datums wird das aktuelle Datum (`datetime.today()`) genommen und mittels `timedelta` die gewünschte Zeitspanne hinzugefügt oder abgezogen.

## Siehe auch:

* Offizielle Python-Dokumentation für datetime: https://docs.python.org/3/library/datetime.html
* Dateutil-Bibliothek: https://dateutil.readthedocs.io/en/stable/
* Arrow-Bibliothek: https://arrow.readthedocs.io/en/latest/
* Pendulum-Bibliothek: https://pendulum.eustace.io/docs/