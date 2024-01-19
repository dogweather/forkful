---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein Datum aus einem String zu lesen, ist die Umwandlung des Datumswerts von einem textbasierten Format in ein Datumsformat, dass vom Computer verstanden wird. Programmierer benötigen dies, um Inhalte wie Log-Dateien, Kalenderereignisse oder Ereignisse im Freitext zu analysieren und für Analysen zu verwenden.

## So geht's:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char buf[255];

    const char* str_date = "2022-10-01 22:35";
    strptime(str_date, "%Y-%m-%d %H:%M", &tm);
    strftime(buf, sizeof(buf), "%d %B %Y, %H:%M", &tm);

    printf("Parsed date: %s\n", buf);

    return 0;
}
```

Wenn Sie das Programm ausführen, liefert es die Ausgabe:
```
Parsed date: 01 October 2022, 22:35
```

## Deep Dive

Die Umsetzung von Datumsstrings in programmverständliche Daten ist keine aktuelle Entwicklung. Seit den frühen Tagen des programmierenden Computings wird diese Funktion benötigt, um menschenlesbare Daten in etwas umzuwandeln, mit dem ein System interagieren kann. 

In C gibt es Bibliotheken wie `time.h`, die Funktionen wie `strptime` und `strftime` bereitstellen, um diese Aufgabe zu erledigen. Es gibt jedoch auch alternative Möglichkeiten, die von früheren C-Versionen oder anderen Plattformen bereitgestellt werden. Diese können ins Spiel kommen, wenn die Umgebung oder der Codekontext die Verwendung von Bibliotheken wie `time.h` nicht erlaubt.

Die Umsetzung selbst erfolgt, indem der String analysiert wird und nach Mustern gesucht wird, die als Datum erkannt werden können. Im obigen Beispiel sucht `strptime` nach den Mustern, die durch die Formatzeichenfolge vorgegeben sind, und füllt die `struct tm` entsprechend.

## Siehe Auch

- strftime - https://www.cplusplus.com/reference/ctime/strftime/
- strptime - https://man7.org/linux/man-pages/man3/strptime.3.html
- C library to make it easy to read and write CSV data - https://github.com/robertpostill/dsv