---
title:                "Python: Vergleich von zwei Datum"
simple_title:         "Vergleich von zwei Datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein grundlegender Aspekt der Programmierung, der uns dabei hilft, bestimmte Datumsangaben zu analysieren oder zu verarbeiten. Dies kann hilfreich sein, um beispielsweise Fristen zu überprüfen oder um zu sehen, ob ein Ereignis in der Vergangenheit oder Zukunft liegt.

## Wie geht man vor

Die Vergleiche von Daten werden mithilfe von speziellen Operatoren durchgeführt. Im Folgenden wird gezeigt, wie man die verschiedenen Arten von Vergleichen durchführt.

### Gleichheit

Um zu prüfen, ob zwei Daten identisch sind, kann der Gleichheitsoperator "=="" verwendet werden. Dieser gibt ein Boolean-Ergebnis zurück, also entweder True oder False, je nachdem ob die Daten gleich oder unterschiedlich sind.

```Python
date1 = "2020-10-05"
date2 = "2020-10-05"

if date1 == date2:
    print("Die Daten sind gleich!")
```

Output: Die Daten sind gleich!

### Größer als/ kleiner als

Um festzustellen, ob ein Datum größer oder kleiner ist als ein anderes Datum, können die Vergleichsoperatoren " > " und " < " verwendet werden. Diese funktionieren ähnlich wie beim Vergleich von Zahlen.

```Python
date1 = "2020-09-01"
date2 = "2020-09-15"

if date2 > date1:
    print("Das zweite Datum liegt in der Zukunft!")
```

Output: Das zweite Datum liegt in der Zukunft!

### Differenz

Um die Differenz zwischen zwei Daten zu berechnen, sollten diese zuerst in ein Datumsformat umgewandelt werden. Anschließend können sie subtrahiert werden, um die Anzahl der Tage dazwischen zu erhalten.

```Python
import datetime

date1 = "2020-07-01"
date2 = "2020-07-10"

difference = datetime.datetime.strptime(date2, "%Y-%m-%d") - datetime.datetime.strptime(date1, "%Y-%m-%d")
print("Die Differenz beträgt", difference.days, "Tage.")
```

Output: Die Differenz beträgt 9 Tage.

## Tiefergehender Einblick

Bei der Verarbeitung von Daten ist es wichtig zu beachten, dass die Reihenfolge der Zahlen in einem Datum von Bedeutung ist. Beim Vergleichen und Berechnen von Daten muss daher immer auf die korrekte Datumsformatierung geachtet werden.

Ein weiterer wichtiger Aspekt beim Vergleichen von Daten ist die Berücksichtigung von Zeitangaben. Dies kann insbesondere bei der Überprüfung von Fristen oder Ereignissen relevant sein.

## Siehe auch

- [Python-Dokumentation: Datetime-Modul](https://docs.python.org/de/3/library/datetime.html)
- [Python-Dokumentation: Vergleichsoperatoren](https://docs.python.org/de/3/reference/expressions.html#comparisons)
- [Real Python Blog: "Working with Datetime Objects in Python"](https://realpython.com/python-datetime/)