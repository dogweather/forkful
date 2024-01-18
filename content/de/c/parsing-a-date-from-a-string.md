---
title:                "Das Parsen eines Datums aus einer Zeichenkette"
html_title:           "C: Das Parsen eines Datums aus einer Zeichenkette"
simple_title:         "Das Parsen eines Datums aus einer Zeichenkette"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn du als Programmierer mit Datumswerten arbeitest, kann es sein, dass du sie aus einer Zeichenkette (String) extrahieren musst. Dieser Vorgang wird als "Datum aus einer Zeichenkette parsen" bezeichnet. Programmierer machen das, um aus einer unstrukturierten Eingabe ein verwendbares Datum zu erhalten.

## Wie geht's?

Das Parsen eines Datums aus einer Zeichenkette ist in C relativ einfach. Du musst lediglich die Funktion `strptime()` aus der Standardbibliothek `time.h` verwenden. Hier ist ein Beispiel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char str[] = "13 March 2021";
    struct tm tm;

    strptime(str, "%d %B %Y", &tm);

    printf("Der %d. Tag im Monat ist ein %s\n", tm.tm_mday, weekday[tm.tm_wday]);

    return 0;
}
```

In diesem Beispiel verwenden wir die `strptime()` Funktion, um das Datum aus der Zeichenkette `str` in die Struktur `tm` zu parsen. Dann greifen wir auf die `tm`-Variablen wie `tm_mday` für den Tag des Monats und `tm_wday` für den Wochentag zu.

Die Ausgabe des obigen Beispiels wäre:

```
Der 13. Tag im Monat ist ein Samstag
```

## Tiefere Einblicke

Geschichtlicher Kontext: Das Parsen von Datumswerten aus Zeichenketten ist seit den Anfängen der Programmierung ein wichtiges Thema. Früher haben Programmierer oft eigene Funktionen geschrieben, um diese Aufgabe zu erledigen. Mit den modernen Funktionen wie `strptime()` ist es jedoch einfacher geworden.

Alternativen: Wenn du keine Datumsangaben in deiner Zeichenkette hast oder dein Datumsformat nicht von `strptime()` unterstützt wird, kannst du auch auf andere Funktionen wie `sscanf()` oder `strtok()` zurückgreifen.

Implementierungsdetails: Die `strptime()` Funktion erwartet eine Zeichenkette mit dem zu parsenden Datum und ein Format, das spezifiziert, wie das Datum strukturiert ist. Es gibt verschiedene Platzhalter, z.B. `%d` für den Tag des Monats und `%B` für den Monatsnamen. Schau dir die [Dokumentation](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time) für eine vollständige Liste an.

## Siehe auch

- [Dokumentation für `strptime()`](https://www.gnu.org/software/libc/manual/html_node/Formatted-Time-Strings.html)
- [Weitere Möglichkeiten zur Formatierung von Datums- und Zeitwerten in C](https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm)
- [Die `strtok()` Funktion](https://www.geeksforgeeks.org/strtok-strtok_r-functions-c-examples/)
- [Die `sscanf()` Funktion](https://www.cplusplus.com/reference/cstdio/sscanf/)