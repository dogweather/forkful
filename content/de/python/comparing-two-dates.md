---
title:                "Vergleich von zwei Datum"
html_title:           "Python: Vergleich von zwei Datum"
simple_title:         "Vergleich von zwei Datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was ist das und warum?: 
Das Vergleichen von zwei Datumsangaben ist ein häufiger Prozess in der Programmierung, bei dem die Ähnlichkeit oder Unterschiede zwischen zwei Zeitpunkten festgestellt werden. Programmierer verwenden diese Technik, um zu prüfen, ob bestimmte Ereignisse in einer bestimmten Reihenfolge stattfinden oder um zu überprüfen, ob ein bestimmtes Datum bereits vergangen ist.

## Wie gehts?: 
Ein Beispiel dafür, wie man zwei Datumsangaben in Python vergleicht, könnte wie folgt aussehen:

```
# Importieren der datetime Bibliothek
import datetime

# Definieren von zwei Datumsangaben
erstes_datum = datetime.date(2020, 10, 15)
zweites_datum = datetime.date(2020, 10, 20)

# Vergleichen der Datumsangaben
if erstes_datum == zweites_datum:
    print("Die Datumsangaben sind gleich.")
elif erstes_datum > zweites_datum:
    print("Das erste Datum ist später als das zweite Datum.")
else:
    print("Das zweite Datum ist später als das erste Datum.")
```

Die Ausgabe dieses Codes wäre: "Das zweite Datum ist später als das erste Datum."

## Tiefer gehend: 
Das Vergleichen von Datumsangaben ist seit langem ein wichtiger Bestandteil der Programmierung. In früheren Versionen von Python wurden dafür die Module "time" und "calendar" verwendet, aber mittlerweile werden Datumsangaben in der Standardbibliothek "datetime" abgedeckt. Es gibt auch Alternativen zu datetime wie zum Beispiel "arrow" oder "dateutil".

Beim Vergleichen von Datumsangaben müssen verschiedene Faktoren beachtet werden, wie z.B. Zeitzone und Differenz in Zeitzonen. Außerdem ist es wichtig zu verstehen, dass Datumsangaben als Objekte behandelt werden und daher speziellen Methoden unterliegen.

## Siehe auch: 
- Offizielle Python Dokumentation zu Datumsangaben: https://docs.python.org/de/3/library/datetime.html
- Dokumentation für "arrow": https://arrow.readthedocs.io/en/latest/
- Dokumentation für "dateutil": https://dateutil.readthedocs.io/en/stable/