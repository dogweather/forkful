---
title:                "C: Die aktuelle Datum erhalten"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum zu erhalten, kann für viele Anwendungen hilfreich sein, sei es für die Organisation von Dateien, die Überprüfung von Garantiefristen oder einfach nur, um den Tag nicht zu vergessen. In dieser Blog-Post werden wir uns anschauen, wie man dies in C programmieren kann.

## Wie

Um das aktuelle Datum in C zu erhalten, müssen wir die Header-Datei ```<time.h>``` importieren. Diese enthält die Funktionen, die wir benötigen, um das Datum und die Uhrzeit zu erhalten.

Zunächst müssen wir eine Variable vom Typ ```struct tm``` deklarieren. Diese wird verwendet, um die aufgeschlüsselten Informationen des Datums (Jahr, Monat, usw.) zu speichern. Dann rufen wir die ```time()``` Funktion auf, die uns die Anzahl an Sekunden seit dem 1. Januar 1970 zurückgibt. Diese Zahl können wir dann an die ```gmtime()``` Funktion übergeben, die dann die entsprechenden Werte in unserer ```struct tm``` Variable aktualisiert. Schließlich können wir diese Werte in unserem Programm beliebig verwenden. Hier ist ein Beispielcode:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm current_date; // Deklariere eine Variable vom Typ struct tm
    time_t seconds = time(NULL); // Rufe die time() Funktion auf und speichere die Rückgabe in seconds
    current_date = *gmtime(&seconds); // Aktualisiere die Werte in der current_date Variable
    printf("Heute ist der %d.%d.%d\n", current_date.tm_mday, current_date.tm_mon+1, current_date.tm_year+1900); // Gib das aktuelle Datum aus (ACHTUNG: tm_mon beginnt bei 0)
    return 0;
}
```

Mit dem obigen Code können wir das aktuelle Datum im Format Tag.Monat.Jahr ausgeben. Hier ist ein Beispieloutput:

```
Heute ist der 12.9.2021
```

## Deep Dive

Die ```time()``` Funktion gibt uns die Anzahl an Sekunden seit dem 1. Januar 1970 zurück, auch bekannt als Unix-Zeitstempel. Dies ist eine gemeinsame Methode, um Datum und Uhrzeit in vielen Programmiersprachen zu speichern, da es eine einfache und standardisierte Möglichkeit bietet, damit zu arbeiten.

```gmtime()``` gibt uns die Werte in Koordinierter Weltzeit (UTC) zurück. Wenn du die lokale Zeit deines Standorts benötigst, kannst du die ```localtime()``` Funktion anstelle von ```gmtime()``` verwenden.

Es ist auch möglich, das aktuelle Datum in einem bestimmten Format auszugeben, indem man die Funktion ```strftime()``` verwendet. Diese erlaubt es uns, ein benutzerdefiniertes Datumsformat anzugeben, wie zum Beispiel ```"%A, %d %b %Y"```, welches uns den Wochentag, den Tag, die abgekürzte Monatsbezeichnung und das Jahr zurückgibt. Hier ist ein Beispielcode:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm current_date;
    time_t seconds = time(NULL);
    current_date = *gmtime(&seconds);
    char date_string[50]; // Deklariere ein Array für die Ausgabe
    strftime(date_string, 50, "%A, %d %b %Y", &current_date); // Verwende strftime() um das Datum in einem bestimmten Format auszugeben
    printf("%s", date_string);
    return 0;
}
```

Hier ist der entsprechende Beispieloutput:

```
Sunday, 12 Sep 2021
```

Für eine vollständige Liste der möglichen Formatierungsoptionen kannst du die [offizielle Dokumentation](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time-Strings.html) der ```strftime()``` Funktion konsultieren.

## Siehe auch

- [Unix-Zeitstempel auf Wikipedia](https://de.wikipedia.org/wiki/Unixzeit)
- [Offizielle Dokumentation der <time.h> Header-Datei](https://www.gnu.org/software/libc/manual/html_node/Time_002dRelated-Types.html#Time_002dRelated-Types)