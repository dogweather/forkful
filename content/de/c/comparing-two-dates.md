---
title:                "Vergleich von zwei Daten"
date:                  2024-01-20T17:32:54.329003-07:00
model:                 gpt-4-1106-preview
simple_title:         "Vergleich von zwei Daten"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten bedeutet, zu überprüfen, welches Datum früher oder später liegt. Programmierer führen solche Vergleiche durch, um Zeitabläufe, Fälligkeiten oder Event-Reihenfolgen zu managen.

## So geht's:
Hier siehst du, wie man zwei `struct tm` Objekte in C vergleicht. Der Einfachheit halber nehmen wir an, dass beide Daten in der selben Zeitzone sind.

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // mktime konvertiert tm in time_t für einfacheren Vergleich
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    if (t1 < t2) {
        return -1; // date1 ist früher als date2
    } else if (t1 > t2) {
        return 1; // date1 ist später als date2
    } else {
        return 0; // Daten sind gleich
    }
}

int main() {
    struct tm date1 = { .tm_year=123, .tm_mon=9, .tm_mday=5 };
    struct tm date2 = { .tm_year=123, .tm_mon=10, .tm_mday=20 };

    int result = compare_dates(date1, date2);
    printf("Ergebnis des Vergleichs: %d\n", result);

    return 0;
}
```

Beispielausgabe:

```
Ergebnis des Vergleichs: -1
```

## Tiefere Einblicke
Früher war Datumvergleich komplizierter. Ohne Standardbibliotheken mussten Programmierer jeden Teil des Datums einzeln vergleichen. Die Bibliothek `time.h` vereinfacht dies enorm durch die Bereitstellung von `struct tm` und Zeitkonvertierungsfunktionen wie `mktime`. Alternativ kann `difftime` genutzt werden, um die Differenz zwischen zwei `time_t` Werten zu bekommen. Bei der Implementierung werden Zeitzonen und Schaltjahre von `mktime` automatisch berücksichtigt, was eine potenzielle Fehlerquelle eliminiert.

## Siehe auch:
- C Standard Library Documentation zu `time.h`: https://en.cppreference.com/w/c/chrono
- Tutorials zu Zeit- und Datumverarbeitung in C: https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm
- ISO C Working Group Website für detaillierte Sprachspezifikationen: http://www.open-std.org/jtc1/sc22/wg14/
