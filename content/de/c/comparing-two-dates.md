---
title:    "C: Vergleich von zwei Daten"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum 

Vergleich von zwei Daten ist ein häufiges Problem in der Programmierung, vor allem, wenn es um das Überprüfen der Gültigkeit von Eingaben oder das Sortieren von Daten geht. Mit dem richtigen Code können Sie sicherstellen, dass Ihre Anwendung korrekte Ergebnisse liefert und effizient arbeitet.

## Wie man es macht 

Um zwei Daten zu vergleichen, können Sie die Funktion `difftime()` verwenden. Hier ist ein Beispiel, das das aktuelle Datum mit einem bestimmten Datum vergleicht:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t now = time(NULL); // aktuelles Datum
  struct tm date = { .tm_year = 2020, .tm_mon = 12, .tm_mday = 31 }; // bestimmtes Datum
  
  double diff = difftime(now, mktime(&date)); // Differenz in Sekunden
  
  if (diff > 0) {
    printf("Das bestimmte Datum liegt in der Vergangenheit.\n");
  } else if (diff < 0) {
    printf("Das bestimmte Datum liegt in der Zukunft.\n");
  } else {
    printf("Das bestimmte Datum ist heute.\n");
  }
  
  return 0;
}
```

**Ausgabe:** Das bestimmte Datum liegt in der Zukunft.

Sie können auch eine benutzerfreundliche Ausgabe für die Differenz in Tagen, Stunden oder Minuten erstellen:

```C
// Umrechnung in Tage
int days = diff / (24 * 60 * 60);

// Umrechnung in Stunden
int hours = (diff - (days * 24 * 60 * 60)) / (60 * 60);

// Umrechnung in Minuten
int minutes = (diff - (days * 24 * 60 * 60) - (hours * 60 * 60)) / 60;

printf("Die Differenz beträgt %d Tage, %d Stunden und %d Minuten.\n", days, hours, minutes);
```

**Ausgabe:** Die Differenz beträgt 365 Tage, 0 Stunden und 0 Minuten.

## Tiefer Einblick 

Die Funktion `difftime()` gibt die Differenz zwischen zwei Daten als Wert vom Typ `double` zurück. Um sicherzustellen, dass die Ausgabe korrekt ist, müssen Sie die Daten in den Typ `time_t` konvertieren. Dies kann mit der Funktion `mktime()` und einer `struct tm`-Variablen erfolgen, die Jahr, Monat, Tag, Stunde, Minute und Sekunde enthält.

Es ist auch wichtig zu beachten, dass die Funktion nur die Differenz zwischen zwei Daten in Sekunden berechnen kann und keine anderen Zeiteinheiten. Um also die Differenz in Tagen, Stunden oder Minuten zu erhalten, müssen Sie diese von der Sekundendifferenz abziehen.

## Siehe auch 

- <https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm>
- <https://www.geeksforgeeks.org/c-program-to-print-last-digit-of-given-value/>