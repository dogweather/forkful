---
title:                "Das aktuelle Datum erhalten"
html_title:           "C: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Es ist oft notwendig, das aktuelle Datum in einem C-Programm zu erhalten. Zum Beispiel kann es benötigt werden, um Zeitstempel für Dateien zu erstellen oder um bestimmte Aufgaben zu automatisieren. Glücklicherweise bietet C eine einfache Möglichkeit, das aktuelle Datum abzurufen.

## Wie bekommt man das aktuelle Datum
Um das aktuelle Datum in C zu erhalten, können wir die Funktion `time()` aus der Standardbibliothek `time.h` nutzen. Diese Funktion gibt die Anzahl der Sekunden seit dem 1. Januar 1970 zurück, auch bekannt als Unix-Zeit.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL); // time(NULL) gibt die aktuelle Zeit zurück
    printf("Das aktuelle Datum ist %ld Sekunden seit dem 1. Januar 1970.\n", now);
    return 0;
}
```

Dieser Code gibt die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 aus. Um das Datum im üblichen Format zu erhalten, können wir die Funktion `localtime()` verwenden, die das `struct tm` zurückgibt. Mit diesem können wir dann einzelne Informationen wie Jahr, Monat, Tag usw. abrufen.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    struct tm *timeinfo = localtime(&now);
    printf("Das aktuelle Datum ist %d.%d.%d\n", timeinfo->tm_mday, timeinfo->tm_mon+1, timeinfo->tm_year+1900);
    return 0;
}
```

Dieser Code gibt das aktuelle Datum im Format Tag.Monat.Jahr aus. Der `+1` und `+1900` sind notwendig, da die Werte in `struct tm` entsprechend verschoben sind.

## Tiefere Einblicke
Wenn wir uns die Funktion `time()` genauer ansehen, ist sie als Typ `time_t` deklariert, was eigentlich ein Alias für einen Ganzzahltyp ist. Diese Ganzzahl repräsentiert die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970. Dadurch kann sie auch zur Berechnung von zukünftigen oder vergangenen Datumswerten verwendet werden.

Eine weitere interessante Funktion ist `asctime()`, die das aktuelle Datum und die Uhrzeit als Zeichenkette im Format "day month year hour:minute:seconds" zurückgibt.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    char *date_string = asctime(localtime(&now));
    printf("Das aktuelle Datum und die Uhrzeit sind: %s\n", date_string);
    return 0;
}
```

## Siehe auch
- [Die Funktion time() in C](https://www.geeksforgeeks.org/time-function-in-c/)
- [C-Bibliothek: `time.h`](https://www.techonthenet.com/c_language/standard_library_functions/time_h/index.php)