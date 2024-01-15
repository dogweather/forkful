---
title:                "Eine Zukunft oder Vergangenheitsdatum berechnen"
html_title:           "C: Eine Zukunft oder Vergangenheitsdatum berechnen"
simple_title:         "Eine Zukunft oder Vergangenheitsdatum berechnen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in deinen C-Programmen mit Datumsangaben arbeitest, möchtest du vielleicht auch in der Lage sein, ein Datum in der Zukunft oder Vergangenheit zu berechnen. Das kann nützlich sein, um beispielsweise eine Benachrichtigung für ein bestimmtes Datum in der Zukunft zu programmieren oder um zu überprüfen, ob ein Datum in der Vergangenheit liegt.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, kannst du die Funktionen aus der Zeitbibliothek "time.h" in deinem C-Code verwenden. Hier ist ein Beispiel, wie du ein Datum 10 Tage in der Zukunft berechnen kannst:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Aktuelles Datum
  time_t now;
  // In 10 Tagen
  time_t tenDays = 10 * 24 * 60 * 60; 
  // Berechne Datum in der Zukunft
  now = time(NULL) + tenDays;
  // Formatierung des Datums
  printf("In 10 Tagen ist der %s", asctime(localtime(&now)));
  return 0;
}
```

**Output:**
```
In 10 Tagen ist der Tue Aug 24 11:25:33 2021
```

Du kannst auch ein Datum in der Vergangenheit berechnen, indem du einfach die Anzahl an Tagen mit einem Minuszeichen versiehst, wie in diesem Beispiel:

```C
time_t now;
time_t tenDays = -10 * 24 * 60 * 60; 
now = time(NULL) + tenDays;
printf("Vor 10 Tagen war es der %s", asctime(localtime(&now)));
```

**Output:**
```
Vor 10 Tagen war es der Mon Aug 09 11:25:33 2021
```

## Deep Dive

Um genauer zu verstehen, wie die Funktionen aus der Zeitbibliothek "time.h" arbeiten, kannst du dir die Dokumentation und verschiedene Tutorials zu diesem Thema anschauen. Es ist auch wichtig zu beachten, dass das Ergebnis der Berechnung von Zeitangaben von verschiedenen Faktoren wie z.B. der Zeitzone abhängen kann.

## Siehe auch

- [Offizielle Dokumentation zu "time.h"](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Tutorial zur Arbeit mit Zeitangaben in C](https://www.geeksforgeeks.org/time-functions-in-c-c/)