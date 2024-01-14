---
title:                "Javascript: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe in der Programmierung. Es ist wichtig, die Länge eines Strings zu kennen, um beispielsweise die Eingabe eines Benutzers zu überprüfen oder die Ausgabe einer bestimmten Funktion zu bestimmen. In diesem Blogpost werden wir uns ansehen, wie man die Länge eines Strings in Javascript finden kann.

## Wie

Um die Länge eines Strings in Javascript zu finden, gibt es eine integrierte Funktion namens `length`. Diese Funktion gibt die Anzahl der Zeichen in einem String zurück. Schauen wir uns ein Beispiel an:

```Javascript
let string = "Hallo Welt";
console.log(string.length);
```

Der obige Code gibt die Ausgabe `11` zurück, da es 11 Zeichen in dem String "Hallo Welt" gibt. Es ist wichtig zu beachten, dass Leerzeichen und Satzzeichen auch als Zeichen gezählt werden.

Man kann auch die Länge eines Strings in einer Variablen speichern und später verwenden:

```Javascript
let string = "Hallo Welt";
let length = string.length;
console.log(length);
```

Dieser Code gibt ebenfalls die Ausgabe `11` zurück.

## Deep Dive

Um die Länge eines Strings genau zu verstehen, müssen wir ein bisschen tiefer in die Grundlagen der Javascript-Programmierung eintauchen. Jeder String in Javascript hat Eigenschaften und Methoden, die man abrufen und verwenden kann. Die `length`-Eigenschaft, die wir zuvor verwendet haben, ist eine dieser Methoden.

Es ist wichtig zu beachten, dass Javascript Unicode unterstützt, was bedeutet, dass ein einzelner Zeichen in einem String aus mehr als einem Byte bestehen kann. Deshalb kann die tatsächliche Länge eines Strings, der Unicode-Zeichen enthält, abweichen von der Anzahl der Zeichen, die durch die `length`-Funktion zurückgegeben wird.

Zum Beispiel hat der String "👋🏽" eine Länge von 1, obwohl er aus zwei Unicode-Zeichen besteht (Handwinken Emoji und Modifikator für Hautfarbe). Dies liegt daran, dass Javascript die Länge eines Strings basierend auf der Anzahl der Unicode-Einheiten berechnet, nicht auf der Anzahl der tatsächlichen Zeichen.

## Siehe auch

- [MDN Web Docs - String.prototype.length](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Tutorialspoint - Javascript String length property](https://www.tutorialspoint.com/javascript-string-length-property)
- [W3Schools - Javascript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)