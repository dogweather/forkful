---
title:                "C: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumsangaben ist ein wichtiger Bestandteil der Programmierung, insbesondere wenn es um die Handhabung von Terminen und Zeitleisten geht. Durch das Vergleichen von zwei Datumswerten können Sie zum Beispiel überprüfen, ob ein Termin in der Zukunft liegt oder ob ein bestimmtes Datum bereits in der Vergangenheit liegt.

## Wie geht man vor

Um zwei Datumswerte in C zu vergleichen, gibt es verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung der Funktion "difftime", die Teil der Standard-C-Bibliothek ist. Diese Funktion berechnet die Differenz zwischen zwei Datumsangaben in Sekunden und gibt entweder einen positiven, negativen oder neutralen Wert zurück, je nachdem welches Datum später liegt.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Zwei Beispieldaten
    struct tm date1 = { .tm_year = 2020, .tm_mon = 5, .tm_mday = 2 };
    struct tm date2 = { .tm_year = 2020, .tm_mon = 4, .tm_mday = 1 };

    // Berechnen der Differenz zwischen den Daten in Sekunden
    double diff = difftime(mktime(&date1), mktime(&date2));

    // Ausgabe des Ergebnisses
    if (diff > 0)
    {
        printf("Date 1 liegt später als Date 2.");
    }
    else if (diff < 0)
    {
        printf("Date 2 liegt später als Date 1.");
    }
    else
    {
        printf("Date 1 ist gleich Date 2.");
    }

    return 0;
}
```

Dieses Beispiel nutzt die Funktion "mktime", um die Datumsangaben in einen numerischen Wert umzuwandeln, der von "difftime" verwendet werden kann. Das Ergebnis ist je nach Situation entweder positiv, negativ oder neutral.

## Tiefere Einblicke

Beim Vergleichen von Datumsangaben ist es wichtig zu beachten, dass es in der Realität viele verschiedene Zeitzonen und Regionen gibt. Daher kann es zu unerwarteten Ergebnissen kommen, wenn Sie nur die Funktion "difftime" verwenden. Eine bessere Möglichkeit ist die Verwendung der "tm_gmtoff"-Strukturkomponente von "tm", die die Anzahl der Sekunden in der Zeitzone des Datums enthält.

Die Arbeit mit Datumswerten kann auch kompliziert werden, wenn es um Berechnungen mit Schaltjahren und Daten in unterschiedlichen Kalenderformaten geht. In solchen Fällen müssen Sie möglicherweise auf externe Bibliotheken oder spezielle Funktionen von C zurückgreifen, um zuverlässige Ergebnisse zu erhalten.

## Siehe auch

- Offizielle Dokumentation von C: https://de.wikibooks.org/wiki/C-Programmierung:_Einf%C3%BChrung
- Verwenden von Datums- und Zeitfunktionen in C: https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm
- Unix-Zeit und ihre Nutzung in C: https://de.wikipedia.org/wiki/Unix-Zeit