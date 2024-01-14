---
title:                "Arduino: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt mit der Extraktion von Teilstrings in der Arduino Programmierung beschäftigen? Nun, in vielen Fällen kann es notwendig sein, nur einen Teil eines längeren Texts zu verwenden oder zu bearbeiten, anstatt den gesamten Text zu verarbeiten. Die Extraktion von substrings ermöglicht es uns, dies effizient und genau zu tun.

## Wie geht das?
Die Extraktion von substrings in der Arduino Programmierung ist relativ einfach. Zunächst müssen wir jedoch einen Text oder eine Zeichenkette festlegen, aus der wir einen Teil extrahieren möchten. In unserem Beispiel nehmen wir an, dass wir den Satz "Hallo, mein Name ist Max" haben und wir möchten nur den Teil "Max" extrahieren. Dies kann mit der `substring()` Funktion erfolgen, die ein Startindex und eine Länge als Parameter akzeptiert.

 ```Arduino
String text = "Hallo, mein Name ist Max";
String substr = text.substring(18, 3); //18 ist der Startindex und 3 ist die Länge
Serial.println(substr); //Gibt "Max" aus
```

Wir definieren also zuerst eine Variable `text` mit unserem Satz und eine weitere Variable `substr`, in der wir den extrahierten Teil speichern. Dann verwenden wir die `substring()` Funktion und geben den Startindex als 18 und die Länge als 3 an. Schließlich geben wir die `substr` Variable mit der `Serial.println()` Funktion aus, um das Ergebnis auf dem seriellen Monitor anzuzeigen.

## Tiefergehende Informationen
Neben dem Extrahieren von Teilstrings kann die `substring()` Funktion auch verwendet werden, um Teilstrings innerhalb eines bestimmten Bereichs zu suchen und zu ersetzen. Sie akzeptiert auch negative Startindizes, was bedeutet, dass wir den Teilstring von hinten zählen können.

Es ist auch wichtig zu beachten, dass die Länge des extrahierten Teilstrings nicht unbedingt der angegebenen Länge entsprechen muss. Wenn die Länge größer ist als der verfügbare Text, wird einfach der Rest des Textes extrahiert.

## Siehe auch
- [Arduino String Dokumentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Substring-Funktion in C++](https://www.cplusplus.com/reference/string/string/substr/)