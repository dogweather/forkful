---
title:                "C: Vergleich von zwei Daten"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
Das Vergleichen von zwei Daten kann in der Programmierung nützlich sein, um festzustellen, welches Datum früher oder später ist. Dies kann hilfreich sein, um bestimmte Aufgaben oder Berechnungen abhängig von bestimmten Daten durchzuführen.

## Wie Geht's
Das Vergleichen von zwei Daten in C wird mithilfe des Vergleichsoperators "==" durchgeführt. Der Operator vergleicht zwei Variablen und gibt entweder 1 (wahr) oder 0 (falsch) zurück. Wir können auch andere Vergleichsoperatoren wie ">", "<" oder ">=" verwenden, um den Vergleich zwischen zwei Daten durchzuführen.

```C
#include <stdio.h>

int main()
{
    int date1 = 25;
    int date2 = 30;

    if(date1 == date2)
    {
        printf("Beide Daten sind gleich.");
    }
    else if(date1 > date2)
    {
        printf("Datum 1 ist später als Datum 2.");
    }
    else
    {
        printf("Datum 1 ist früher als Datum 2.");
    }
    return 0;
}
```

Output:
```
Datum 1 ist früher als Datum 2.
```

## Tiefer Einblick
In C werden Daten hinter den Kulissen als Ganzzahlen dargestellt. Wenn wir zwei Daten vergleichen, werden sie als Ganzzahlen interpretiert und dann der Vergleichsoperator angewendet. Dies macht es möglich, zwischen anderen Datentypen wie Zeichenketten und Dezimalzahlen zu vergleichen, indem sie in Ganzzahlen umgewandelt werden.

## Siehe Auch
- [Vergleichsoperatoren in C](https://www.programiz.com/c-programming/c-operators)
- [Datentypen in C](https://www.learn-c.org/en/Data_Types)
- [Bedingte Ausdrücke in C](https://www.cprogramming.com/tutorial/c/lesson2.html)