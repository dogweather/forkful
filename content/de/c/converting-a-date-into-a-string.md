---
title:                "C: Ein Datum in eine Zeichenfolge umwandeln"
simple_title:         "Ein Datum in eine Zeichenfolge umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Umwandeln eines Datums in eine Zeichenkette mag auf den ersten Blick vielleicht keine spannende Aufgabe erscheinen, aber es ist eine wichtige Fähigkeit für jeden C-Programmierer. Das Konvertieren von Daten in verschiedene Formate, wie zum Beispiel in eine Zeichenkette, ist eine grundlegende Technik in der Programmierung und wird häufig benötigt, um Daten für die Ausgabe oder die Verarbeitung zu formatieren.

## Wie

Das Konvertieren eines Datums in eine Zeichenkette in C ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir die Variablen für das Datum erstellen, die wir konvertieren möchten. In diesem Beispiel verwenden wir die Standard-Library-Funktion `time()` um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten. Dann verwenden wir die `strftime()` Funktion, um das Datum in eine Zeichenkette umzuwandeln.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Variablen für das Datum und die Zeichenkette erstellen
    time_t now;
    struct tm *date;
    char date_str[50];

    // Aktuelles Datum und Uhrzeit abrufen
    time(&now);
    
    // Datum in eine Zeichenkette umwandeln
    date = localtime(&now);
    strftime(date_str, sizeof(date_str), "%d.%m.%Y", date);

    // Zeichenkette ausgeben
    printf("%s", date_str);

    return 0;
}
```

Die `strftime()` Funktion erwartet drei Argumente: die Variable, in der das umgewandelte Datum gespeichert werden soll, die maximale Größe dieser Variable und das Format, in dem das Datum ausgegeben werden soll. Im obigen Beispiel verwenden wir das Format `%d.%m.%Y`, um das Datum in dem üblichen deutschen Format anzuzeigen: Tag, Monat, Jahr.

Wenn wir dieses Programm ausführen, sollte die Ausgabe das aktuelle Datum in Form einer Zeichenkette im gewünschten Format enthalten.

```
31.03.2021
```

## Deep Dive

Obwohl das Konvertieren eines Datums in eine Zeichenkette in C einfach erscheinen mag, gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen wir sicherstellen, dass die Variable, in der das umgewandelte Datum gespeichert wird, groß genug ist, um das gesamte Datum abzudecken. Andernfalls könnte die Zeichenkette abgeschnitten werden und das Datum wäre unvollständig.

Außerdem bietet die `strftime()` Funktion viele verschiedene Formatierungsoptionen, mit denen wir das Datum nach unseren Wünschen formatieren können. Wir können auch spezifische Sprach- und Ländercodes angeben, um das Datum in verschiedenen regionalen Formaten anzuzeigen.

Eine vollständige Liste der verfügbaren Formatierungsoptionen und ihre Bedeutung kann der offiziellen Dokumentation der `strftime()` Funktion entnommen werden.

## Siehe auch

- Offizielle Dokumentation der `strftime()` Funktion: https://www.cplusplus.com/reference/ctime/strftime/
- Weitere Informationen zu Datums- und Zeitformaten in C: https://www.tutorialspoint.com/cprogramming/c_date_time.htm