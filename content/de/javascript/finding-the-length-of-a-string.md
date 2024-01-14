---
title:                "Javascript: Die Länge eines Strings finden"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Fähigkeit, die jeder Javascript Entwickler beherrschen sollte. Es ermöglicht uns, die Anzahl der Zeichen in einem Text zu bestimmen, was in vielen Programmieraufgaben hilfreich ist.

## Wie man die Länge eines Strings findet

Um die Länge eines Strings in Javascript zu finden, können wir die Methode `.length` verwenden. Diese Methode gibt uns die Anzahl der Zeichen in einem String zurück. Schauen wir uns ein Beispiel an:

```Javascript
let string = "Hallo";
console.log(string.length);
```

Dieses Beispiel wird "5" ausgeben, da "Hallo" insgesamt fünf Zeichen hat. Wir können auch eine Variable verwenden, um einen String zu speichern und dann die Länge dieses Strings zu finden:

```Javascript
let string = "Ich bin ein String!";
let length = string.length;
console.log(length);
```

In diesem Beispiel wird "18" ausgegeben, da der String 18 Zeichen lang ist. Wir können die Methode `.length` auch auf andere Datentypen anwenden, wie zum Beispiel auf Arrays oder Objekte. Hier ist ein Beispiel mit einem Array:

```Javascript
let array = [1, 2, 3, 4, 5];
console.log(array.length);
```

Dieses Beispiel wird "5" ausgeben, da das Array insgesamt fünf Elemente hat.

## Tiefergehende Informationen

Obwohl es einfach erscheinen mag, die Länge eines Strings zu finden, gibt es doch einige wichtige Dinge zu beachten. Zum Beispiel zählen Leerzeichen und Satzzeichen auch als Zeichen in einem String. Das bedeutet, dass ein String wie "Hallo!" eine Länge von sechs hat, da das Ausrufezeichen als ein Zeichen gezählt wird.

Eine weitere wichtige Sache zu beachten ist, dass die Methode `.length` auf Unicode basiert. Das bedeutet, dass einige Zeichen, wie zum Beispiel ä, ü oder ö, nicht als einzelnes Zeichen gezählt werden, sondern als zwei Zeichen. Dies kann zu unerwarteten Ergebnissen führen, wenn man die Methode `.length` auf Zeichen mit diesen speziellen Zeichen anwendet.

Wenn wir die Länge eines Strings finden, werden wir manchmal auch eines Zeichens überführt (englisch: truncate), was bedeutet, dass wir den String auf eine bestimmte Länge kürzen. Hier ist ein Beispiel, wie wir dies mit der Methode `.slice()` tun können:

```Javascript
let string = "Dieser String ist viel zu lang.";
let newString = string.slice(0, 10);
console.log(newString); // Ausgabe: "Dieser Str"
```

Hier haben wir die ersten zehn Zeichen des Strings in eine neue Variable gespeichert. Auf diese Weise können wir die Länge unseres Strings auf eine bestimmte Anzahl von Zeichen begrenzen.

## Siehe auch

- [W3Schools - String Length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [MDN Web Docs - String.length](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Javascript.info - String length](https://javascript.info/string/length)