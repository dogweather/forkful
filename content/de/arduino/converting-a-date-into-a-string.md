---
title:                "Arduino: Ein Datum in einen String umwandeln"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Sie möglicherweise Datumswerte in Ihrem Arduino-Programm in eine Zeichenkette umwandeln möchten. Zum Beispiel können Sie die Daten an ein Display anhängen, in eine Datei schreiben oder sie mit anderen Daten über Serial Communication senden.

## Wie man

Es gibt verschiedene Möglichkeiten, dies in Ihrem Arduino-Code zu erreichen. Eine Möglichkeit besteht darin, die Daten in eine Zeichenkette zu konvertieren, indem Sie die `itoa()` Funktion verwenden. Diese Funktion konvertiert eine ganze Zahl in eine Zeichenkette. Schauen wir uns ein Beispiel an:

```Arduino
int day = 15;
int month = 10;
int year = 2020;

char date[11]; // Zeichenkette mit 11 Zeichen: DD.MM.YYYY\0
// "date" muss groß genug sein, um die gesamte Zeichenfolge aufzunehmen

itoa(day, date, 10); // "day" wird in eine Zeichenkette konvertiert und in "date" gespeichert
date[2] = '.'; // fügt einen Punkt nach den ersten beiden Zeichen hinzu

itoa(month, date + 3, 10); // "month" wird in eine Zeichenkette konvertiert und direkt an "date" angehängt, beginnend bei Index 3
date[5] = '.'; // fügt einen Punkt nach den nächsten beiden Zeichen hinzu

itoa(year, date + 6, 10); // "year" wird in eine Zeichenkette konvertiert und direkt an "date" angehängt, beginnend bei Index 6
```

Die Ergebniszeichenkette lautet: `15.10.2020`

Wenn Sie das Datum mit den aktuellen Zeitwerten aktualisieren möchten, können Sie die `strftime()` Funktion verwenden. Diese Funktion wandelt das Datum in eine formatierte Zeichenkette um, basierend auf dem von Ihnen angegebenen Format. Schauen wir uns auch hierzu ein Beispiel an:

```Arduino
#include <time.h> // time library importieren

int day;
int month;
int year;

char date[11]; // Zeichenkette mit 11 Zeichen: DD.MM.YYYY\0
// "date" muss groß genug sein, um die gesamte Zeichenfolge aufzunehmen

time_t current_time = time(nullptr); // aktuelle Zeit als Unix-Zeitstempel abrufen
struct tm * current_tm = localtime(&current_time); // Unix-Zeitstempel in ein "tm" struct umwandeln, das Datum und Uhrzeit enthält

// das Format für unsere Date-String ist: "DD.MM.YYYY"
strftime(date, sizeof(date), "%d.%m.%Y", current_tm); // das Datum wird in das "date" Array formatiert, basierend auf dem angegebenen Format
```

Die Ergebniszeichenkette lautet: `15.10.2020`

## Tief eintauchen

Wenn Sie tiefer in das Konzept der Datums- und Zeichenkettenkonvertierung einsteigen möchten, gibt es einige Dinge zu beachten. Zum Beispiel müssen Sie sicherstellen, dass das Array, in dem Sie die Zeichenkette speichern, groß genug ist, um die gesamte Zeichenfolge aufzunehmen. Andernfalls können Sie Speicherfehler oder unerwartete Ergebnisse erhalten.

Es ist auch wichtig zu verstehen, dass die `char` und `String` Datentypen unterschiedlich sind. Zeichenketten können als Arrays von Zeichen betrachtet werden, während `String` ein objektartiger Datentyp ist, der zusätzliche Funktionen bereitstellt. Wenn Sie also eine Zeichenkette in Ihrem Code verwenden möchten, achten Sie darauf, den richtigen Datentyp zu verwenden.

## Siehe auch

- [itoa() Referenz](https://www.arduino.cc/reference/en/language/functions/conversion/itoa/)
- [strftime() Referenz](https://www.cplusplus.com/reference/ctime/strftime/)
- [Char vs. String: Was ist der Unterschied?](https://www.arduino.cc/reference/en/language/variables/data-types/string/)