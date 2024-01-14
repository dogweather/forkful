---
title:                "Arduino: Extrahieren von Teilstrings"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum 

In der Arduino-Programmierung gibt es viele nützliche Funktionen, die dabei helfen, komplexe Aufgaben mit Leichtigkeit zu bewältigen. Eine dieser Funktionen ist das Extrahieren von Teilzeichenketten oder Teilstrings aus einer größeren Zeichenkette. Diese Funktion ist besonders hilfreich, wenn man nur bestimmte Teile einer Zeichenkette benötigt, anstatt die gesamte Zeichenkette zu verarbeiten.

## Wie man Teilzeichenketten extrahiert 

Die Arduino-Programmiersprache bietet eine eingebaute Funktion namens `substring()`, die es ermöglicht, Teilzeichenketten zu extrahieren. Diese Funktion erfordert zwei Parameter: den Startindex und die Anzahl der zu extrahierenden Zeichen. Um die `substring()`-Funktion zu verwenden, muss man zuerst eine Zeichenkette erstellen und sie einer Variablen zuweisen. Schauen wir uns ein Beispiel an, um dies besser zu verstehen:

```Arduino
String zeichenkette = "Hallo Welt";
String teilzeichenkette = zeichenkette.substring(6, 4);
```

In diesem Beispiel wird eine Zeichenkette namens "Hallo Welt" erstellt und einer Variablen namens `zeichenkette` zugewiesen. Dann wird mithilfe der `substring()`-Funktion eine Teilzeichenkette aus der Variable `zeichenkette` extrahiert. Der erste Parameter `6` gibt den Startindex an, an dem die Teilzeichenkette beginnen soll (in diesem Fall das Leerzeichen zwischen den Wörtern "Hallo" und "Welt"). Der zweite Parameter `4` gibt an, wie viele Zeichen extrahiert werden sollen (in diesem Fall 4 Zeichen, was "Welt" ausgibt). Die Teilzeichenkette wird dann der Variablen `teilzeichenkette` zugewiesen.

Die Ausgabe dieses Beispiels wäre "Welt", da die `substring()`-Funktion die Teilzeichenkette ab dem Startindex bis zum angegebenen Anzahl an Zeichen extrahiert.

## Tiefergehende Informationen 

Beim Extrahieren von Teilzeichenketten sollte man einige Dinge beachten. Der Startindex sollte immer kleiner als die Länge der Zeichenkette sein, da sonst ein Fehler auftritt. Darüber hinaus ist es wichtig zu beachten, dass der Startindex bei 0 beginnt, nicht bei 1. Das heißt, der erste Buchstabe hat den Index 0, der zweite den Index 1 und so weiter. Wenn man also beispielsweise den Buchstaben "o" in der Zeichenkette "Hallo Welt" extrahieren möchte, muss man den Index 4 angeben, nicht 5.

## Siehe auch 

- [Die offizielle Arduino-Dokumentation zu `substring()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Ein Tutorial zum Extrahieren von Teilzeichenketten in der Arduino-Programmierung](https://howtomechatronics.com/tutorials/arduino/extracting-a-substring-from-a-string-in-arduino/)

Vielen Dank fürs Lesen! Wir hoffen, dass dieser Artikel Ihnen geholfen hat, zu verstehen, wie man Teilzeichenketten in der Arduino-Programmierung extrahiert. Happy coding!