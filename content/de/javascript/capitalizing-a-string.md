---
title:                "String in Großbuchstaben umwandeln"
html_title:           "Javascript: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren von Strings ist einfach gesagt die Umwandlung des ersten Buchstabens eines Wortes in einen Großbuchstaben. Programmierer tun dies, um Strings besser lesbar zu machen oder für stilistische Zwecke.

## Wie geht's?
Das Kapitalisieren von Strings ist in Javascript einfach und unkompliziert. Hier ist ein Beispiel:
```Javascript
let string = "hallo welt";
console.log(string.charAt(0).toUpperCase() + string.slice(1));
```
Die Ausgabe wird "Hallo welt" sein. Der erste Buchstabe "h" wurde mit der `toUpperCase()` Methode in einen Großbuchstaben umgewandelt, und der Teil ab dem zweiten Buchstaben wurde mit der `slice()` Methode wieder an den ursprünglichen String angehängt.

## Tief einsteigen
Das Kapitalisieren von Strings ist ein grundlegendes Konzept in der Programmierung und wird in vielen Sprachen verwendet. Ursprünglich wurde dies verwendet, um die erste Buchstabe eines Namens oder Wortes in der Sprache Englisch zu betonen. Heutzutage wird es jedoch hauptsächlich für stilistische Zwecke verwendet, da die meisten Computerprogramme auf Groß- und Kleinschreibung nicht empfindlich sind.

Bei der Umwandlung von Strings in Großbuchstaben gibt es auch alternative Methoden, wie z.B. die `toLocaleUpperCase()` Methode, die internationale Zeichen unterstützt. Auch die Verwendung von Regular Expressions ist möglich, um mehrere Wörter in einem String zu kapitalisieren.

Die Implementierung des Kapitalisierens von Strings ist in Javascript aufgrund der eingebauten Methoden relativ einfach. In anderen Sprachen oder in situationsabhängigen Fällen kann es jedoch erforderlich sein, benutzerdefinierten Code zu schreiben.

## Siehe auch
- [String.prototype.toUpperCase() Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Alternativen zur Großschreibung von Strings](https://www.geeksforgeeks.org/javascript-string-prototype-touppercase-method/)
- [Verschiedene Methoden zum Manipulieren von Strings in Javascript](https://www.tutorialspoint.com/What-is-the-difference-between-capitalize-and-uppercase-in-JavaScript)