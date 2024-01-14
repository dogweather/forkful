---
title:                "Python: Das Berechnen eines Datums in der Zukunft oder Vergangenheit."
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Manchmal müssen wir als Programmierer*innen bestimmte Daten in die Zukunft oder Vergangenheit berechnen. Dies kann notwendig sein, um bspw. geplante Termine oder Geburtstage zu erfassen.

## Wie geht's
Die Berechnung von Datumsangaben in Python ist relativ einfach. Zunächst müssen wir das Modul "datetime" importieren.

```
import datetime
```

Um ein Datum in der Zukunft zu berechnen, können wir die Funktion "date.fromisoformat" verwenden und das gewünschte Datum im ISO-Format eingeben.

```
zukunfts_datum = datetime.date.fromisoformat('2021-12-31')
```

Anschließend können wir mit der Funktion "timedelta" die Differenz zwischen dem aktuellen Datum und dem zukünftigen Datum berechnen.

```
differenz = zukunfts_datum - datetime.date.today()
```

Um das Ergebnis in Tagen, Wochen oder Monaten anzuzeigen, können wir die entsprechenden Methoden "days", "weeks" oder "months" verwenden.

```
print(differenz.days) # Output: 336
print(differenz.days / 7) # Output: 48
print(differenz.months) # Output: 11
```

Um ein Datum in der Vergangenheit zu berechnen, können wir den gleichen Prozess anwenden. Statt mit der aktuellen Zeit zu berechnen, verwenden wir jedoch ein beliebiges Datum in der Vergangenheit.

```
vergangenes_datum = datetime.date.fromisoformat('1990-01-01')
differenz = datetime.date.today() - vergangenes_datum
```

## Tiefer eintauchen
Um ein tieferes Verständnis für die Berechnung von Datumswerten in Python zu bekommen, empfiehlt es sich, die offizielle Dokumentation zu lesen. Dort werden auch weitere Methoden und Optionen erläutert, die bei der Arbeit mit Datumswerten hilfreich sein können.

## Siehe auch
- [Offizielle Dokumentation von Python zu Datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial zu Daten und Zeiten in Python](https://realpython.com/python-datetime/)
- [Tipps für die Arbeit mit Datumswerten in Python](https://www.blog.pythonlibrary.org/2018/01/24/python-101-working-with-dates-and-time/)