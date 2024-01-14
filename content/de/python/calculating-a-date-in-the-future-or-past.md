---
title:    "Python: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum
In diesem Blog Post werden wir uns damit beschäftigen, wie man in Python ein bestimmtes Datum in der Zukunft oder Vergangenheit berechnen kann. Dies ist besonders hilfreich, wenn man zum Beispiel ein Programm schreibt, das bestimmte Ereignisse oder Termine im Voraus planen muss.

## How To
Um ein Datum in der Zukunft oder Vergangenheit in Python zu berechnen, verwenden wir das Modul "datetime". Zunächst müssen wir dieses Modul importieren:

```Python
import datetime
```

Um ein spezifisches Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Funktion "timedelta" verwenden. Diese Funktion erwartet einen numerischen Wert als Argument, der die Anzahl der Tage darstellt, um die das Datum verschoben werden soll. Wir können auch Wochen, Monate oder Jahre angeben, indem wir das entsprechende Argument vor dem numerischen Wert angeben.

Als Beispiel berechnen wir das Datum in 30 Tagen:

```Python
# Heutiges Datum erhalten
today = datetime.date.today()

# Datumsdifferenz von 30 Tagen berechnen
date_in_30_days = today + datetime.timedelta(days=30)

# Ausgabe
print(date_in_30_days)
```

Die Ausgabe wird je nach heutigem Datum unterschiedlich sein, aber sie wird immer 30 Tage in der Zukunft liegen.

Um ein Datum in der Vergangenheit zu berechnen, können wir einfach ein negatives Argument für "timedelta" verwenden. Als Beispiel berechnen wir das Datum vor 2 Monaten:

```Python
# Heutiges Datum erhalten
today = datetime.date.today()

# Datumsdifferenz von 2 Monaten berechnen
date_2_months_ago = today - datetime.timedelta(months=2)

# Ausgabe
print(date_2_months_ago)
```

Die Ausgabe wird wieder je nach heutigem Datum unterschiedlich sein, aber sie wird immer 2 Monate in der Vergangenheit liegen.

## Deep Dive
Um tiefer in das Thema einzutauchen, können wir uns anschauen, wie Python mit Datumswerten umgeht. In Python sind Datumswerte als Objekte des Typs "date" gespeichert, die aus den Komponenten Jahr, Monat und Tag bestehen. Das Modul "datetime" stellt auch die Funktionen "today()" und "now()" zur Verfügung, um das heutige Datum und die aktuelle Uhrzeit zu erhalten.

Es ist auch erwähnenswert, dass Python automatisch den Februar berücksichtigen und das richtige Datum für Schaltjahre berechnen kann, wenn wir zusätzliche Argumente wie Monate oder Jahre angeben.

## See Also
- [Python-Dokumentation zum Modul "datetime"](https://docs.python.org/3/library/datetime.html)
- [Artikel zu Schaltjahren in Python](https://www.digitalocean.com/community/tutorials/how-to-find-leap-years-in-python-3)