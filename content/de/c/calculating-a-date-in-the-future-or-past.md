---
title:                "C: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum

Das Berechnen von zukünftigen oder vergangenen Daten ist in der Programmierung oft notwendig, um beispielsweise Fristen oder Termine zu überprüfen oder automatisch zu aktualisieren.

# Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, wird die Bibliothek <time.h> in C verwendet. Diese enthält Funktionen und Strukturen, die es ermöglichen, mit Datum und Uhrzeit in C zu arbeiten.

Im Folgenden finden Sie ein Beispiel, das zeigt, wie man ein Datum in der Vergangenheit berechnen kann:

```C
#include <stdio.h>
#include <time.h>

int main(void) {
  // Aktuelles Datum erhalten
  time_t current_time;
  time(&current_time);

  // Umwandlung des aktuellen Datums in eine Struktur
  struct tm* current_date = localtime(&current_time);

  // Datum für 1 Jahr in der Vergangenheit berechnen
  current_date->tm_year -= 1;

  // Umwandlung der berechneten Struktur in einen Zeitstempel
  time_t past_time = mktime(current_date);

  // Ausgabe des zukünftigen Datums
  printf("Ein Jahr in der Vergangenheit war es am: %s", ctime(&past_time));
  return 0;
}
```

Die Ausgabe des oben genannten Beispiels wäre folgende:

```
Ein Jahr in der Vergangenheit war es am: So Jan  6 18:02:57 2019
```

Um ein Datum in der Zukunft zu berechnen, würde man die entsprechende Zeile ändern, um beispielsweise `current_date->tm_year += 1` zu berechnen.

# Tiefer eintauchen

Die Bibliothek <time.h> ist sehr umfangreich und bietet viele weitere Funktionen und Strukturen. So kann man beispielsweise auch die Wochentage oder die Anzahl der Tage in einem Monat berechnen.

Eine wichtige Sache, die man beachten sollte, ist die Verwendung der richtigen Zeitzone. In den meisten Fällen wird <time.h> die lokale Zeitzone verwenden, aber dies kann geändert werden, indem man die Funktion `setenv("TZ", "Europe/Berlin", 1)` vor der Verwendung anderer Funktionen aufruft. Hierbei sollte natürlich die entsprechende Zeitzone für den jeweiligen Standort angegeben werden.

# Siehe auch

- Offizielle Dokumentation der Bibliothek <time.h>: https://en.cppreference.com/w/c/chrono
- Ein umfassender Tutorial über die Verwendung von Datum und Uhrzeit in C: https://www.tutorialspoint.com/cprogramming/c_date_time.htm
- Eine Liste von Zeitzone-Abkürzungen: https://www.timeanddate.com/time/zones/