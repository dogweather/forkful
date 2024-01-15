---
title:                "Ein String großschreiben"
html_title:           "Arduino: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Kapitalisieren von Strings ist ein häufiger Schritt beim Programmieren, besonders wenn es um die Formatierung von Text oder die Interaktion mit externen Geräten geht. Durch das Verständnis dieses einfachen Konzepts können Sie in der Lage sein, Ihre Arduino-Programme effizienter und effektiver zu gestalten.

## Wie es geht

Grundsätzlich bedeutet Kapitalisieren, den ersten Buchstaben eines Wortes in einen Großbuchstaben zu verwandeln. In Arduino gibt es verschiedene Möglichkeiten, dies zu erreichen. Im Folgenden zeigen wir Ihnen zwei Methoden, um einen String zu kapitalisieren:

### Methode 1
```Arduino
String input = "arduino";
input.toUpperCase(); // konvertiert den String in "ARDUINO"
```

### Methode 2
```Arduino
String output = "";
String input = "hello world";
for (int i = 0; i < input.length(); i++) { // durchläuft jeden Buchstaben im String
  output += toupper(input[i]); // konvertiert den aktuellen Buchstaben in einen Großbuchstaben und fügt ihn zum Ausgabe-String hinzu
}
// Ausgabe: "HELLO WORLD"
```

## Tief eintauchen

Es gibt viele weitere Möglichkeiten, einen String in Arduino zu manipulieren, einschließlich der Anwendung von Funktionen wie `toLowerCase()` und `replace()`. Sie können auch ein Array von Charakteren verwenden, um individuell auf jeden Buchstaben im String zuzugreifen und ihn zu verändern. Das Wichtigste ist jedoch, dass Sie die grundlegende Idee des Kapitalisierens verstehen und anwenden können.

## Siehe auch

- [Arduino String Referenz](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Stringfunktionen in Arduino](https://www.tutorialspoint.com/arduino/arduino_string_functions.htm)
- [String Manipulation mit Arduino](https://www.youtube.com/watch?v=82EIoEDKcfs)