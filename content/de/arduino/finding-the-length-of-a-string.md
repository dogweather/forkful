---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erfassen der Länge eines Strings gibt die Anzahl der Zeichen im String an. Es ist brandwichtig, weil, ohne diese Information, Programmierer keine angemessene Kontrolle über ihre Funktionen und Bedingungen hätten.

## So geht's:

Arduino macht es uns einfach, die Länge eines Strings zu finden, dank der Funktion `length()`. Lassen Sie uns ein Beispiel betrachten:

```Arduino
String meinText = "Hallo Welt";
int laenge = meinText.length();
Serial.println(laenge);
```

In diesem Code initialisieren wir einen String 'meinText' und verwenden dann die length()-Methode, um seine Länge zu erfassen. Die Ausgabe ist '10' - Es bedeutet, die Länge des Strings "Hallo Welt" ist 10.

## Tiefer Eintauchen:

Historisch hat die Länge eines Strings in der Programmierung oft die Basis für viele Bedingungen und Funktionen gebildet. Im Gegensatz zu einigen älteren Programmiersprachen, bietet Arduino eine eingebaute Methode, um die Länge eines Strings zu finden, was es effizienter und benutzerfreundlicher zu handhaben macht.

Alternativen dazu würden darin bestehen, einen manuellen Ansatz zur Zählung der Zeichen zu verfolgen - aber dies kann nervenaufreibende Arbeit sein und zu Fehlern führen, da es anfällig für Fehleinschätzung ist.

Als Implementierungsdetail verfolgt die Arduino `length()` Methode einen einfachen Ansatz. Es durchläuft jeden Charakter des Strings einmal und erhöht einen Zähler, bis das Ende des Strings erreicht ist.

## Siehe auch:

1. Arduino Referenz: [Strings](https://www.arduino.cc/reference/de/language/variables/data-types/string/)
2. Arduino Referenz: [String.length()](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/length/)