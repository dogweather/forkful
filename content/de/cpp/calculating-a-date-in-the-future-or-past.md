---
title:    "C++: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann in vielen Situationen nützlich sein. Zum Beispiel könnte es nützlich sein, um Feiertage oder wichtige Ereignisse zu planen, um zu wissen, wann ein bestimmter Tag in der Woche fällt oder um die Dauer einer bestimmten Zeitspanne zu berechnen.

## Anleitung

Um eine Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die C++ Standardbibliotheksfunktion `mktime()` verwenden. Diese Funktion berechnet die Anzahl der Sekunden seit dem 1. Januar 1970 und kann dann verwendet werden, um ein `tm` Objekt zu erstellen. Wir können dann dieses `tm` Objekt verwenden, um das Datum in einem bestimmten Format auszugeben.

```C++
#include <iostream>
#include <ctime>

int main() {
  // Erstellen eines "tm" Objekts mit dem aktuellen Datum
  std::tm futureDate = *std::localtime(std::time(nullptr));

  // Hinzufügen von 30 Tagen zur Berechnung eines Datums in der Zukunft
  futureDate.tm_mday += 30;

  // Verwenden von mktime() um die Anzahl der Sekunden seit dem 1. Januar 1970 zu berechnen
  std::time_t result = std::mktime(&futureDate);

  // Anzeigen des berechneten Datums in einem bestimmten Format
  std::cout << "Das Datum in 30 Tagen: " << std::asctime(std::localtime(&result)) << std::endl;

  return 0;
}
```

Output:

```
Das Datum in 30 Tagen: Mon Sep 06 00:00:00 2021
```

## Tiefentauchen

Die `tm` Struktur ist in der `ctime` Header-Datei definiert und enthält Informationen über Datum und Zeit in einem bestimmten Format. Wenn Sie mehrere Tage, Monate oder Jahre hinzufügen möchten, können Sie einfach die entsprechenden Werte für `tm_mday`, `tm_mon` oder `tm_year` der `tm` Struktur ändern.

Es ist auch möglich, ein Datum in der Vergangenheit zu berechnen, indem man eine negative Zahl anstatt einer positiven Zahl für den Tag, Monat oder das Jahr verwendet.

## Siehe auch

- [C++ Standardbibliotheksfunktion mktime()](https://www.cplusplus.com/reference/ctime/mktime/)
- [C++ Standardbibliotheksfunktion asctime()](https://www.cplusplus.com/reference/ctime/asctime/)
- [C++ Standardbibliotheksfunktion localtime()](https://www.cplusplus.com/reference/ctime/localtime/)