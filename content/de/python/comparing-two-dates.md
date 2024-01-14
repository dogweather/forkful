---
title:                "Python: Vergleichen von zwei Daten"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung ist es oft notwendig, verschiedene Daten miteinander zu vergleichen. Dies kann zum Beispiel bei der Bestimmung von Zeitspannen oder der Überprüfung von Daten auf Gültigkeit hilfreich sein. In diesem Blogbeitrag werde ich erklären, wie man in Python zwei Datumsangaben vergleichen kann.

## How To

Um zwei Daten in Python zu vergleichen, können wir das Modul `datetime` verwenden. Zunächst müssen wir jedoch zwei Datumsobjekte erstellen, die verglichen werden sollen. Dies können wir mit der Methode `datetime.strptime()` tun, indem wir das entsprechende Datum und das Datumsformat als Argumente übergeben.

```Python
import datetime

date1 = datetime.datetime.strptime('01.01.2021', '%d.%m.%Y')
date2 = datetime.datetime.strptime('31.12.2020', '%d.%m.%Y')
```

Anschließend können wir die Methoden `date1.year`, `date1.month` und `date1.day` verwenden, um auf die einzelnen Teile des Datums zuzugreifen. Mit diesen Werten können wir dann verschiedene Vergleichsoperationen durchführen, zum Beispiel die Gleichheit oder die Reihenfolge der Daten überprüfen.

```Python
if date1 == date2:
    print('Die Daten sind gleich.')

if date1 > date2:
    print('Datum 1 liegt nach Datum 2.')

if date1 < date2:
    print('Datum 1 liegt vor Datum 2.')
```

Die möglichen Vergleichsoperatoren sind `==` (gleich), `!=` (ungleich), `>` (größer), `<` (kleiner), `>=` (größer oder gleich) und `<=` (kleiner oder gleich).

## Deep Dive

Wenn wir genauer betrachten, wie Python Datumsangaben intern speichert, wird deutlich, warum sie so einfach vergleichen können. Python speichert Datumsangaben als Anzahl der vergangenen Tage seit dem 1. Januar 1970 in Form von ganzen Zahlen. Dadurch können Daten einfach als Zahlen miteinander verglichen werden.

Um dies zu verdeutlichen, betrachten wir folgendes Beispiel:

```Python
date1 = datetime.datetime.strptime('01.01.2021', '%d.%m.%Y')
date2 = datetime.datetime.strptime('01.01.2020', '%d.%m.%Y')

print(date1 - date2)
```

Die Ausgabe dieses Codes ist `366 days, 0:00:00`, da ein Schaltjahr dazwischen liegt. Wenn wir jedoch `366 days` in Tagen umrechnen, erhalten wir den Wert `366`, was der Differenz in Tagen zwischen den beiden Daten entspricht. Dadurch wird deutlich, wie Python Datumsangaben vergleicht.

## Siehe auch

- [Python-Dokumentation zu datetime](https://docs.python.org/de/3/library/datetime.html)
- [Python-Datumsformatierung](https://strftime.org/)
- [Vergleichsoperatoren in Python](https://www.python-kurs.eu/python3_operatoren.php)