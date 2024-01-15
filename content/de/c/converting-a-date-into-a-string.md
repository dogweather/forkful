---
title:                "Ein Datum in einen String umwandeln"
html_title:           "C: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Ein Datum in Form eines Strings umzuwandeln, ist eine häufige und wichtige Aufgabe beim Programmieren in C. Es kann dazu dienen, dem Benutzer das Datum in einer leicht lesbaren Form anzuzeigen oder es als Teil von Dateinamen oder Log-Dateien zu verwenden. In diesem Artikel zeigen wir Ihnen, wie Sie dies in C erreichen können.

## So geht's

Um ein Datum in C in einen String umzuwandeln, gibt es mehrere mögliche Ansätze. Eine Möglichkeit ist die Verwendung der Standardfunktion `strftime()`. Hier ist ein Beispiel, das das aktuelle Datum in einem benutzerdefinierten Format ausgibt:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // aktuelles Datum erhalten
    time_t now = time(NULL);

    // Benutzerformat definieren
    char str[100];
    char format[] = "%d.%m.%Y";

    // Datum in String umwandeln mithilfe von strftime()
    strftime(str, 100, format, localtime(&now));

    // Ausgabe
    printf("Das aktuelle Datum ist: %s\n", str);

    return 0;
}
```

Die Ausgabe dieses Codes wird in der Konsole wie folgt aussehen:

```
Das aktuelle Datum ist: 15.12.2021
```

Es gibt auch andere Funktionen in C, die verwendet werden können, um Datum zu Strings zu konvertieren, wie beispielsweise `asctime()` und `ctime()`. Es ist wichtig, die richtige Funktion basierend auf den Anforderungen und dem gewünschten Format auszuwählen.

## Werfen wir einen genaueren Blick darauf

Beim Konvertieren von Datum in Strings gibt es einige wichtige Dinge zu beachten. Zum Beispiel kann das gewünschte Format durch das `format`-Argument in `strftime()` definiert werden. Dieses Argument erlaubt verschiedene spezifische Formatierungen für Datum, Uhrzeit und Wochentag, die verwendet werden können, um den String anzupassen.

Eine weitere wichtige Überlegung ist die Verwendung der richtigen Zeitzone, um die korrekte Zeit zu erhalten. Hier haben wir die Funktion `localtime()` verwendet, um das Datum in der lokalen Zeitzone auszugeben. Wenn Sie jedoch ein bestimmtes Datum in einer anderen Zeitzone benötigen, müssen Sie möglicherweise andere Funktionen wie `gmtime()` oder `tzset()` verwenden.

## Siehe auch

- [String von Datum in C erstellen - Beispiel](https://www.programiz.com/c-programming/library-function/strftime)
- [Datum in C ausgeben mit Systemfunktionen](https://www.tutorialspoint.com/c_standard_library/c_function_asctime.htm)
- [C Datum und Uhrzeit Funktionen](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)