---
title:    "C: Die aktuelle Datum bekommen"
keywords: ["C"]
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums kann in vielen Anwendungsfällen nützlich sein, wie zum Beispiel bei der Erstellung von Dateien oder bei der Verwaltung von Zeitstempeln. Die Verwendung von C-Programmierung ermöglicht es uns, diese Funktionalität in unsere Anwendungen zu integrieren.

# Wie man das aktuelle Datum abruft

Um das aktuelle Datum in C zu erhalten, müssen wir die Funktion `time_t time (time_t *timer)` verwenden. Diese Funktion gibt die aktuelle Zeit in der Zeitvariable `timer` zurück.
```C
#include <stdio.h>
#include <time.h>

int main() {
   time_t my_time;
   my_time = time(NULL);
   
   printf("Das aktuelle Datum und die Uhrzeit sind: %s", ctime(&my_time));
   
   return 0;
}
```
Das obige Beispiel gibt das aktuelle Datum und die Uhrzeit im folgenden Format aus: `Day Mon DD HH:MM:SS YYYY`. Hier sind einige Beispiele für die Ausgabe:
```
Die aktuelle Zeit und das Datum sind: Wed Jun 12 17:15:50 2019
```
```
Die aktuelle Zeit und das Datum sind: Tue Dec 01 11:45:30 2020
```

# Tiefere Einblicke

Um das aktuelle Datum genauer zu verstehen, ist es wichtig, ein Grundverständnis für die Datums- und Zeitrechnung zu haben. In der C-Programmierung wird das Datum normalerweise als Anzahl der Sekunden seit dem 01.01.1970, 00:00 Uhr UTC dargestellt. Dieser Punkt in der Zeit wird auch als "Epoch-Zeit" bezeichnet. Die Funktion `time()` gibt eine `time_t`-Variable zurück, die diese Anzahl von Sekunden enthält. Mit dieser Information können wir das Datum und die Uhrzeit im gewünschten Format ausgeben, wie im vorherigen Abschnitt gezeigt.

# Siehe auch

- [Ein Leitfaden zur C-Programmierung](https://www.tutorialspoint.com/cprogramming/index.htm)
- [Die Referenzseite für die `time()`-Funktion in C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Eine Erklärung der Epoch-Zeit](https://de.wikipedia.org/wiki/Unixzeit)