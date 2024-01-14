---
title:                "Arduino: Löschen von Zeichen, die mit einem Muster übereinstimmen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Warum:

Manchmal ist es notwendig, bestimmte Zeichen in einem Text zu löschen, die einem bestimmten Muster entsprechen. Das kann zum Beispiel der Fall sein, wenn man Daten aus einer seriellen Schnittstelle empfängt und sie in einer bestimmten Form verarbeiten möchte. In solchen Fällen ist es hilfreich, einen Algorithmus zu haben, der diese Aufgabe automatisiert.

Wie Geht’s:

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, gibt es verschiedene Ansätze. In diesem Blogpost werden wir uns auf die Verwendung der Funktion `String::remove()` in der Arduino IDE konzentrieren. Diese Funktion ermöglicht es uns, Zeichen aus einem String zu entfernen, die einem bestimmten Muster entsprechen.

```Arduino
String text = "Hallo Welt!";
text.remove("a");  // Entfernt alle "a" aus dem String
Serial.println(text); // Ausgabe: Hllo Welt!
```

In diesem Beispiel wird das Zeichen "a" aus dem String `text` entfernt und die modifizierte Version wird über die serielle Schnittstelle ausgegeben.

Tipp: Die `String::remove()` Funktion kann auch mit anderen Datentypen wie `char` oder `int` verwendet werden, indem diese zuvor in einen String umgewandelt werden.

```Arduino
char letter = 'e';
String text = "Hallo Welt!";
text.remove(String(letter)); // Entfernt alle "e" aus dem String
Serial.println(text); // Ausgabe: Hallo Wlt!
```

Eine weitere Möglichkeit ist die Verwendung von Schleifen, um jeden einzelnen Buchstaben in einem String zu überprüfen und bei Übereinstimmung zu löschen.

```Arduino
String text = "Hallo Welt!";
int length = text.length();

// Durchlaufe jeden Buchstaben im String
for(int i = 0; i < length; i++){
  // Überprüfe auf Übereinstimmung mit dem gesuchten Muster
  if(text.charAt(i) == 'a'){
    // Lösche den Buchstaben an dieser Stelle
    text.remove(i);
  }
}

Serial.println(text); // Ausgabe: Hallo Welt!
```

Tipp: Mit der `String::indexOf()` Funktion können Sie auch die Position eines bestimmten Zeichens im String finden und es dann mit `String::remove()` entfernen.

Deep Dive:

Zusätzlich zu den hier vorgestellten Methoden gibt es noch weitere Möglichkeiten, um Zeichen aus einem String zu löschen, die einem bestimmten Muster entsprechen. Dazu gehören die Verwendung von regulären Ausdrücken oder die Verwendung von Bibliotheken wie der `String` oder `TextFinder` Bibliothek.

Es ist auch wichtig zu beachten, dass die Verwendung von `String` Objekten auf dem Arduino oft zu Speicherproblemen führen kann, da der Arduino nur begrenzten Speicher hat. Daher ist es ratsam, sparsam mit `String` Objekten umzugehen und stattdessen eher auf Zeichenarrays zurückzugreifen.

Siehe auch:

- [Offizielle Arduino Referenz für die String::remove() Funktion](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/remove/)
- [Beispiele für die Verwendung von String::remove() in der Arduino IDE](https://www.arduino.cc/en/Tutorial/StringRemove)
- [Verwendung von regulären Ausdrücken auf dem Arduino](https://www.robotic-controls.com/learn/arduino/arduino-regex-expression-regular-expressions)
- [Verwendung der String Bibliothek auf dem Arduino](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [Verwendung der TextFinder Bibliothek auf dem Arduino](https://github.com/adafruit/Adafruit_TextFinder)