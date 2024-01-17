---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Arduino: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erkennen der Länge einer Zeichenfolge ist ein wichtiger Prozess in der Programmierung. Es bezieht sich auf die Anzahl der Zeichen, die in einer Zeichenfolge enthalten sind. Programmierer tun dies, um die Zeichenfolge effizient zu verarbeiten und zu manipulieren.

## Wie geht das?
```Arduino
String string = "Hallo Welt";
int length = string.length();
Serial.println(length); // Ausgabe: 11
```

## Tiefergehende Infos
Zurückführend auf die Anfänge der Programmiersprache C, ist die Funktion `strlen()` die bekannteste Methode zur Bestimmung der Länge einer Zeichenfolge. Alternativen wie `sizeof()` oder `strnlen()` haben jeweils ihre Vor- und Nachteile. In Arduino werden die Zeichenfolgen als Objekte der Klasse `String` behandelt, wodurch die Methode `length()` verwendet werden kann.

## Siehe auch
- [Tutorial: Strings in Arduino](https://www.arduino.cc/en/Tutorial/String)
- [Funktion strlen() in C](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Alternative Methoden zur Erkennung der Länge einer Zeichenfolge](https://stackoverflow.com/questions/7923583/what-is-the-difference-between-strlen-and-sizeof)