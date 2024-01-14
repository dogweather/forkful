---
title:                "TypeScript: Die Länge eines Strings finden"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wer kennt es nicht? Man möchte in seinem Code die Länge eines Textes ermitteln, sei es zum Validieren einer Eingabe oder zur Berechnung von Abständen. Doch wie macht man das in TypeScript? In diesem Artikel werden wir uns damit beschäftigen, wie man die Länge eines Strings in TypeScript finden kann.

## Wie

Um die Länge eines Strings in TypeScript zu ermitteln, gibt es eine einfache Methode namens `length`. Diese wird auf den String aufgerufen und gibt die Anzahl der Zeichen zurück. Schauen wir uns das an einem Beispiel an:

```TypeScript
const text: string = "Hallo, Welt!";
const length: number = text.length;

console.log(length);
// Output: 12
```
Wie man sieht, werden alle Zeichen, einschließlich Leerzeichen, gezählt. Die Methode `length` kann auf jede Art von String in TypeScript angewendet werden, sei es ein einzelner Buchstabe oder ein ganzer Text.

## Deep Dive

Wenn man sich den TypeScript Code genauer anschaut, wird man bemerken, dass die Methode `length` in Wirklichkeit eine Eigenschaft von `String` ist. Diese gibt die Anzahl der Zeichen einer Zeichenkette zurück. Dabei müssen jedoch Unicode-Zeichen als mehr als ein Zeichen gezählt werden, was zu einer höheren Länge führt. Es ist außerdem wichtig zu beachten, dass die Methode `length` nur für lesende Operationen genutzt werden kann, das heißt, man kann sie nicht zur Modifikation eines Strings verwenden.

## Siehe auch

- [Offizielle TypeScript Dokumentation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-length)
- [JavaScript String length property](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [JavaScript String objects](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)