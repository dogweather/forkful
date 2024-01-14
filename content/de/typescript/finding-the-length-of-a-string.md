---
title:                "TypeScript: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist ein grundlegender Bestandteil der Programmierung mit TypeScript. Es ermöglicht uns, die Anzahl der Zeichen einer Zeichenkette zu bestimmen und sie entsprechend zu manipulieren.

## Wie geht das

Um die Länge einer Zeichenkette in TypeScript zu finden, können wir die `length`-Eigenschaft verwenden. Diese liefert uns die Anzahl der Zeichen einer Zeichenkette zurück.

```TypeScript
let string = "Hallo";
console.log(string.length);
// Output: 5
```

Wir können die `length`-Eigenschaft auch auf mehrzeilige Zeichenketten anwenden. Hierbei werden auch die Leerzeichen mitgezählt.

```TypeScript
let multilinestring = `Hallo
Welt`;
console.log(multilinestring.length);
// Output: 11
```

## Tiefere Einblicke

In TypeScript werden Zeichenketten als Instanzen des `string`-Datentyps behandelt. Dieser Datentyp verfügt über die `length`-Eigenschaft, die uns die Anzahl der Zeichen einer Zeichenkette liefert.

Die `length`-Eigenschaft ist in TypeScript auch bei anderen Datentypen wie zum Beispiel Arrays und Maps verfügbar. Bei Arrays gibt sie uns die Anzahl der Einträge zurück, während sie bei Maps die Anzahl der Schlüssel-Wert-Paare liefert.

## Siehe auch

- [TypeScript Dokumentation zur length-Eigenschaft](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Einführung in TypeScript](https://www.typescriptlang.org/docs/handbook/typescript-from-scratch.html)
- [Manipulation von Zeichenketten in TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)