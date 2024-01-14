---
title:                "Python: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Daten ist eine nützliche Fähigkeit für Programmierer, die in vielen Projekten nützlich sein kann. Egal ob es um die Planung von Events oder das Erstellen von Zeitreihen geht, das Berechnen von Datumsangaben kann den Umgang mit Datumswerten erleichtern.

## Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zunächst das `datetime` Modul von Python importieren. Dann können wir die Funktion `date` verwenden, um ein Datum zu erstellen und mit Hilfe von `timedelta` können wir eine Anzahl an Tagen für die Berechnung angeben.

```Python
import datetime

zukunfts_datum = datetime.date(2021, 10, 1)
vergangenheits_datum = datetime.date(2018, 3, 15)

differenz = datetime.timedelta(days=30)

zukuenftiges_datum = zukunfts_datum + differenz
vergangenes_datum = vergangenheits_datum - differenz

print(zukuenftiges_datum)
print(vergangenes_datum)
```

Die Ausgabe dieses Codes wären die Daten: `2021-10-31` und `2018-02-15`. Unsere Differenz von 30 Tagen wurde jeweils zu unserem Startdatum addiert bzw. subtrahiert, um die zukünftigen bzw. vergangenen Daten zu erhalten. 

## Tiefer eintauchen

Es ist auch möglich, nicht nur mit Tagen sondern auch mit Stunden, Minuten und Sekunden zu rechnen. Hierfür können wir die Funktion `datetime` verwenden, anstatt `date`.

```Python
import datetime

aktueller_zeitpunkt = datetime.datetime.now()

zukuenftiger_zeitpunkt = aktueller_zeitpunkt + datetime.timedelta(hours=3, minutes=35)
vergangener_zeitpunkt = aktueller_zeitpunkt - datetime.timedelta(minutes=10)

print(zukuenftiger_zeitpunkt)
print(vergangener_zeitpunkt)
```

Die Ausgabe dieses Codes wäre ein zukünftiger Zeitpunkt, der 3 Stunden und 35 Minuten später als der aktuelle Zeitpunkt liegt, sowie ein vergangener Zeitpunkt, der 10 Minuten früher als der aktuelle Zeitpunkt liegt.

## Siehe auch

- Offizielle Dokumentation zum `datetime` Modul: https://docs.python.org/3/library/datetime.html
- Weitere Informationen über die `timedelta` Funktion: https://docs.python.org/3/library/datetime.html#timedelta-objects