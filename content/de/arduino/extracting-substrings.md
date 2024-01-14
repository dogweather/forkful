---
title:    "Arduino: Extrahieren von Teilzeichenketten"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
Das Extrahieren von Teilstrings ist eine nützliche Fähigkeit in der Arduino Programmierung, die es ermöglicht, bestimmte Abschnitte von Strings oder Zeichenketten zu isolieren. Dies kann hilfreich sein, um bestimmte Informationen aus einem größeren Datensatz zu erhalten oder um eine effizientere Verarbeitung von Daten durchzuführen.

## So geht's
Um einen Teilstring in Arduino zu extrahieren, verwenden wir die Funktion `substring()`. Diese Funktion erwartet zwei Parameter: den Startindex und die Länge des zu extrahierenden Teilstrings. Beispiel:
```Arduino
String s = "Hallo Welt";
String sub = s.substring(0, 5); // sub enthält jetzt "Hallo"
```

Um herauszufinden, welcher Teilstring extrahiert werden soll, können wir die Funktion `indexOf()` verwenden, die den Index des ersten Vorkommens eines bestimmten Zeichens oder einer Zeichenkette zurückgibt. Beispiel:
```Arduino
String s = "Berlin, Germany";
int index = s.indexOf(","); // index enthält jetzt den Wert 7
```
Um einen Teilstring ab einem bestimmten Index zu erhalten, können wir die `substring()` Funktion mit dem Index als erstem Parameter aufrufen und die Länge des Teilstrings als zweiten Parameter angeben. Beispiel:
```Arduino
String s = "Berlin, Germany";
String sub = s.substring(0, 6); // sub enthält jetzt "Berlin"
```

## Tiefere Einblicke
Beim Extrahieren von Teilstrings gibt es einige Dinge zu beachten. Zum einen müssen wir sicherstellen, dass der angegebene Index nicht außerhalb der Länge des ursprünglichen Strings liegt. Andernfalls wird ein leerer String zurückgegeben. Außerdem ist es wichtig zu wissen, dass die Indizes in der `substring()` Funktion bei 0 beginnen. Das heißt, der erste Buchstabe hat einen Index von 0 und der letzte Buchstabe hat einen Index, der um 1 kleiner als die Länge des Strings ist.

## Siehe auch
- [String-Referenz für Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [substring() Funktion in der Arduino-Dokumentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [indexOf() Funktion in der Arduino-Dokumentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)