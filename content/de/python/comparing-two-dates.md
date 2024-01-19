---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Vergleichen von zwei Daten bezieht sich auf den Prozess der Bestimmung, welches Datum früher oder später ist. Programmierer tun dies oft, um Zeitrahmen zu bestätigen, Zeitmessungen durchzuführen oder termingebundene Logik innerhalb ihrer Anwendungen zu implementieren.

## So geht's:

```Python
from datetime import datetime

datum_1 = datetime(2022, 5, 1)
datum_2 = datetime(2022, 7, 1)

if datum_1 < datum_2:
  print("Datum 1 ist früher als Datum 2")
else:
  print("Datum 2 ist später als Datum 1")
```

Ergebnisausgabe:

```
Datum 1 ist früher als Datum 2
```

## Deep Dive

Historisch gesehen haben sich Programmierer oft auf Dritt-Bibliotheken wie pytz für Datumsvergleiche verlassen. Seit der Python-Version 3.8 ist dies jedoch durch die eingebaute Funktion datetime() einfacher geworden.

Alternativen zum Vergleichen von Daten in Python könnten andere beliebte Bibliotheken wie dateutil oder arrow sein.

Bei der Implementierung ist zu beachten, dass in Python Datum und Uhrzeit immer in Verbindung mit der Zeitzone betrachtet werden sollten. Ohne spezifizierte Zeitzone wird das Datum und die Uhrzeit in der standardmäßigen Systemzeitzone ausgegeben.

## Siehe Auch

- Python Dokumentation für datetime: https://docs.python.org/3/library/datetime.html
- pytz Bibliothek: https://pypi.org/project/pytz/
- dateutil Bibliothek: https://dateutil.readthedocs.io/en/stable/
- arrow Bibliothek: https://arrow.readthedocs.io/en/latest/