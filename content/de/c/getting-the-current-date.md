---
title:    "C: Das aktuelle Datum abrufen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Jeder kennt das Gefühl, wenn man sich fragt, welcher Tag heute ist. Oder man muss in einem Programm die aktuelle Datum und Uhrzeit anzeigen lassen. Hier kommt die Programmierung ins Spiel, um uns das Leben etwas einfacher zu machen. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man in der Programmiersprache C das aktuelle Datum und Uhrzeit ausgeben kann.

## Wie

Um das aktuelle Datum und Uhrzeit in C zu bekommen, benötigen wir Bibliotheken. Eine davon ist `time.h`, die uns Funktionen zur Manipulation von Datum und Zeit zur Verfügung stellt. Wir müssen sie in unserem Code importieren, indem wir `#include <time.h>` am Anfang unseres Programms hinzufügen.

Als nächstes müssen wir eine Variable vom Typ `time_t` erstellen, um das Datum und die Uhrzeit zu speichern. Dann können wir die Funktion `time()` aufrufen, um den aktuellen Zeitstempel in diese Variable zu speichern.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Variable vom Typ time_t erstellen
    time_t currentTime;
    
    // Aktuellen Zeitstempel in die Variable speichern
    currentTime = time(NULL);

    return 0;
}
```

Jetzt haben wir den aktuellen Zeitstempel bekommen, aber wie können wir ihn in ein lesbares Datum und Uhrzeitformat umwandeln? Dafür gibt es die Funktion `localtime()`, die uns eine Struktur vom Typ `tm` zurückgibt, die Jahr, Monat, Tag, Stunde, Minute und Sekunde enthält. Mit `printf()` können wir nun diese Werte ausgeben und das aktuelle Datum und die Uhrzeit anzeigen.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Variable vom Typ time_t erstellen
    time_t currentTime;
    
    // Aktuellen Zeitstempel in die Variable speichern
    currentTime = time(NULL);
    
    // Struktur vom Typ tm erstellen und Zeitstempel übergeben
    struct tm *currentDateTime = localtime(&currentTime);

    // Ausgabe des aktuellen Datums und Uhrzeit
    printf("Aktuelles Datum: %d.%d.%d\n", currentDateTime->tm_mday, currentDateTime->tm_mon + 1, currentDateTime->tm_year + 1900);
    printf("Aktuelle Uhrzeit: %d:%d:%d\n", currentDateTime->tm_hour, currentDateTime->tm_min, currentDateTime->tm_sec);

    return 0;
}
```

Die Ausgabe sieht dann ungefähr so aus:

```
Aktuelles Datum: 22.04.2021
Aktuelle Uhrzeit: 16:30:00
```

## Deep Dive

Die Funktionen `time()` und `localtime()` sind sehr nützlich, um das aktuelle Datum und die Uhrzeit zu bekommen. Sie verwenden den sogenannten Unix-Epoch-Zeitstempel, der die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 um 00:00 Uhr GMT darstellt. Dieser Zeitstempel wird verwendet, um Datum und Uhrzeit zu berechnen, da sie unabhängig von Zeitzonen und Sommerzeit sind.

Je nach Bedarf kann die Struktur vom Typ `tm` auch bearbeitet werden, um andere Zeitangaben zu erhalten, z.B. die Wochentage oder die Datum und Uhrzeit in einer bestimmten Zeitzonen.

## Siehe auch

- [C-Bibliothek <time.h> (de.wikipedia.org)](https://de.wikipedia.org/wiki/C-Bibliothek_source)
- [C time() Funktion (c-programmieren.com)](https://www.c-programmieren.com/c-programmiersprache/c-funktion-time.php)
- [Gibt es eine Möglichkeit, mit C das aktuelle Datum und die Uhrzeit zu erhalten? (stackoverflow.com)](https://stackoverflow.com/questions/1442116/how-to-get-the-date-and-time-values-in-a-c-program)