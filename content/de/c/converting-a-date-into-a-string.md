---
title:                "C: Eine Datum in einer Zeichenfolge umwandeln"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in eine Zeichenfolge ist ein wichtiger Schritt in der Programmierung, um eine benutzerfreundliche Darstellung von Datum und Zeit zu ermöglichen. Eine solche Funktion ist nützlich, wenn Sie beispielsweise einen Benutzer mit einem bestimmten Datum begrüßen möchten oder eine Formatierung für die Anzeige von Kalenderereignissen benötigen.

## Wie geht's

```C
#include <stdio.h>
#include <time.h>

int main()
{
  time_t jetzt = time(NULL);
  struct tm * datum = localtime(&jetzt);

  // Datumsformat festlegen
  char zeichenfolge[50];
  strftime(zeichenfolge, sizeof(zeichenfolge), "%A, %d. %B %Y", datum);

  printf("Heute ist %s.\n", zeichenfolge);

  return 0;
}
```

**Ausgabe:**

```
Heute ist Freitag, 28. Mai 2021.
```

Dieses Beispiel nutzt die Standardbibliotheken `stdio.h` und `time.h`, um das aktuelle Datum und die aktuelle Uhrzeit abzurufen. Mit der Funktion `strftime()` können wir das Datum in einem bestimmten Format ausgeben.

Um ein individuelles Format zu erstellen, können verschiedene Formatierungszeichen verwendet werden, die in der Dokumentation von `strftime()` aufgeführt sind. In diesem Beispiel haben wir den Wochentag, das Datum, den Monat und das Jahr angegeben.

## Tiefer Einblick

Bei der Konvertierung eines Datums in eine Zeichenfolge gibt es einige wichtige Dinge zu beachten. Zunächst ist die verwendete Zeichenfolge begrenzt auf eine bestimmte Länge. Es ist wichtig sicherzustellen, dass die Zeichenfolge groß genug ist, um das Datum in dem gewünschten Format aufzunehmen.

Darüber hinaus ist es wichtig, die korrekten Funktionen zum Abrufen des Datums und der Uhrzeit zu verwenden, je nachdem, ob Sie das lokale oder das UTC-Datum benötigen. Die Funktionen `localtime()` und `gmtime()` können hier hilfreich sein.

Es sollte auch beachtet werden, dass die Formatierung von Datumsangaben in verschiedenen Regionen unterschiedlich sein kann. Daher ist es wichtig, dass Sie die richtigen Formatierungsmöglichkeiten für Ihre Zielgruppe kennen.

## Siehe auch

- [Dokumentation zu `strftime()`](https://www.cplusplus.com/reference/ctime/strftime/) (Englisch)
- [Formatierung von Datumsangaben in verschiedenen Regionen](https://www.timeanddate.com/date/timezone.html) (Englisch)
- [Beispielcode für das Konvertieren eines Datums in eine Zeichenfolge](https://www.geeksforgeeks.org/how-to-convert-date-string-to-month-integer-in-cc-programming/) (Englisch)