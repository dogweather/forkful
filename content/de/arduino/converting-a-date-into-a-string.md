---
title:                "Arduino: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum du ein Datum in einen String umwandeln solltest

Manchmal kann es nützlich sein, ein Datum in einen String umzuwandeln, besonders wenn man mit Datumswerten arbeitet und sie auf einem Display oder in einer seriellen Kommunikation ausgeben möchte.

# Wie man ein Datum in einen String umwandelt

Um ein Datum in einen String umzuwandeln, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der `String()` Funktion in Arduino. Hier ist ein Beispiel:

```Arduino
int tag = 25;
int monat = 12;
int jahr = 2020;

String datumsString = String(tag) + "." + String(monat) + "." + String(jahr);
```

Dieses Beispiel zeigt, wie man einzelne Datumswerte zu einem String zusammenfügen kann. Der `String()` Befehl konvertiert dabei die Integer-Werte in einen String. Das Ergebnis in diesem Fall wäre "25.12.2020".

Es gibt auch andere Möglichkeiten, wie zum Beispiel die Bibliothek `TimeLib.h`, die speziell für die Arbeit mit Zeit und Datum entwickelt wurde.

# Tiefere Einblicke

Bei der Umwandlung von Datum in einen String müssen einige Dinge beachtet werden, wie zum Beispiel das Format des Strings. In vielen Ländern wird zum Beispiel das Datum in der Reihenfolge Tag-Monat-Jahr geschrieben, während in anderen Ländern das Format Monat-Tag-Jahr üblich ist. Man sollte also sicherstellen, dass man das richtige Format für den jeweiligen Einsatzzweck verwendet.

Außerdem sollte man bei der Verwendung von `String()` Funktion darauf achten, dass man nicht zu viele Strings hintereinander hängt, da dies die Programmausführung verlangsamen kann. Es ist daher immer besser, den `String()` Befehl nur einmal zu verwenden und den String in einer Variablen zu speichern.

# Siehe auch

- [TimeLib.h Library](https://github.com/PaulStoffregen/Time)
- [String() Function Reference](https://www.arduino.cc/reference/de/language/variables/data-types/string/)