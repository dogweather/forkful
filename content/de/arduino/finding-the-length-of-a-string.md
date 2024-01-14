---
title:    "Arduino: Die Länge einer Zeichenkette finden."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings in der Programmierung kann ein nützliches Werkzeug sein, um Textverarbeitung oder Datenanalyse durchzuführen. Mit Arduino kann dies eine wertvolle Funktion sein, um komplexe Anwendungen zu verwalten oder die Kommunikation zwischen verschiedenen Komponenten zu erleichtern.

## Wie geht es

Das Finden der Länge eines Strings auf einem Arduino ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen Sie einen String erstellen, indem Sie einfach einen Text in Anführungszeichen setzen:

```Arduino
String text = "Hallo, Welt!";
```
Anschließend können Sie die "length()" Funktion verwenden, um die Länge des Strings zu ermitteln und in eine Variable zu speichern:

```Arduino
int length = text.length();
```

Schließlich können Sie die Länge des Strings auf dem seriellen Monitor ausgeben, um das Ergebnis zu überprüfen:

```Arduino
Serial.println(length);
```

In diesem Beispiel gibt der serielle Monitor die Ausgabe "12" zurück, da der String "Hallo, Welt!" 12 Zeichen hat.

## Tiefere Einblicke

Die "length()" Funktion verwendet einen einfachen Algorithmus, um die Anzahl der Zeichen in einem String zu zählen. Es beginnt bei 1 und zählt jedes Zeichen im String, bis es das letzte Zeichen erreicht. Diese Funktion kann auch helfen, die Länge eines Arrays zu finden, da Arrays auch als Strings behandelt werden können.

Eine wichtige Sache zu beachten ist, dass diese Funktion aufgrund von Codierung und Speicherbeschränkungen möglicherweise nicht immer genau ist. Es ist immer eine gute Idee, die Ausgabe zu überprüfen und sicherzustellen, dass sie das erwartete Ergebnis liefert.

## Siehe auch

- [Die offizielle Arduino-Dokumentation zur "length()" Funktion](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/length/)
- [Ein interessantes Tutorial zur Arbeit mit Strings auf dem Arduino](https://www.arduino.cc/en/Tutorial/StringLengthTrim)
- [Praktische Beispiele für die Verwendung von "length()" auf dem Arduino](https://www.teachmemicro.com/arduino-string-length/)