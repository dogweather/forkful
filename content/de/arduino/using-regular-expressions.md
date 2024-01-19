---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (oder Regex) sind eine Art Muster, die hilft, Zeichenketten zu matchen, zu suchen, zu ersetzen und zu manipulieren. Programmierer verwenden sie, um Zeit zu sparen und Fehler bei der Bearbeitung von Strings zu vermeiden.

## So geht's:

Um Regex in Arduino zu verwenden, wird eine Bibliothek benötigt. Hier sind einige einfache Beispiele, wie man es macht:

```Arduino
#include <regex.h>

void setup() {
    Serial.begin(9600);
  
    regex_t regex;
  
    if (regcomp(&regex, "a", 0)) {
        Serial.println("Regexp Compile Error");
    }
  
    if (!regexec(&regex, "This is a test.", 0, NULL, 0)) {
        Serial.println("Regexp Match");
    } else {
        Serial.println("Regexp No Match");
    }

    regfree(&regex);
}

void loop() {
}
```

Dieses Programm verwendet den regulären Ausdruck `a`, um nach diesem Buchstaben im gegebenen String zu suchen.

## Vertiefung:

Obwohl reguläre Ausdrücke schon seit Jahrzehnten existieren, sind sie in jüngerer Zeit in vielen Programmiersprachen angekommen. Obwohl Arduino reguläre Ausdrücke nicht nativ unterstützt, gibt es Bibliotheken wie `regex.h` die dies ermöglichen.

Alternativen zu regulären Ausdrücken sind Funktionen wie `strstr()` oder `strtok()`, die aber nicht die gleiche Flexibilität und Benutzerfreundlichkeit bieten.

Wenn es um die Implementierungsdetails geht, ist zu beachten, dass die Regex-Verarbeitung oft zeitaufwendig sein kann, insbesondere auf Mikrocontrollern mit begrenzten Ressourcen.

## Siehe auch:

Weitere Informationen und Beispiele finden Sie unter diesen Links:

1. [C++ Reguläre Ausdrücke (RegEx) - cppreference.com](https://en.cppreference.com/w/cpp/regex)
2. [Arduino-Referenz - Arduino.cc](https://www.arduino.cc/reference/en/)
3. [Reguläre Ausdrücke in C - tutorialspoint.com](https://www.tutorialspoint.com/c_standard_library/c_function_regexec.htm)