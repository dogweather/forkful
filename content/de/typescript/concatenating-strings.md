---
title:                "Verknüpfen von Zeichenketten"
html_title:           "TypeScript: Verknüpfen von Zeichenketten"
simple_title:         "Verknüpfen von Zeichenketten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Szenarien, in denen das Zusammenfügen oder Verketten von Zeichenfolgen in der Programmierung erforderlich ist. Zum Beispiel, um einen zusammenhängenden Satz oder eine Nachricht zu erstellen, oder um Variablen und Werte in einer einzigen Zeichenfolge zu kombinieren, die dann für weitere Operationen verwendet werden kann. Die Fähigkeit, Strings miteinander zu verketten, ist ein grundlegendes Konzept in der Entwicklung von Software und ermöglicht es den Programmierern, dynamische, aussagekräftige und komplexe Ausgaben zu erstellen.

## Wie geht's?

Die Konkatenation von Strings in TypeScript ist sehr einfach und unkompliziert. Um zwei oder mehr Strings miteinander zu verbinden, können Sie den `+` Operator verwenden. Beispiel:

```TypeScript
let string1 = "Hallo ";
let string2 = "Welt!";
let combinedString = string1 + string2;
console.log(combinedString);
```

Das Ergebnis dieses Codes wird sein: `Hallo Welt!`

Sie können auch mehrere Strings miteinander verketten, indem Sie den `+` Operator mehrmals verwenden. Beispiel:

```TypeScript
let string1 = "Mein Name";
let string2 = "ist";
let string3 = "John";
let string4 = "Doe";
let combinedString = string1 + " " + string2 + " " + string3 + " " + string4;
console.log(combinedString);
```

Das Ergebnis dieses Codes wird sein: `Mein Name ist John Doe`

## Deep Dive

In TypeScript und anderen Programmiersprachen werden Strings als Arrays von Zeichen behandelt. Das bedeutet, dass jeder einzelne Buchstabe in einer Zeichenfolge einen spezifischen Index hat und somit aufgerufen und manipuliert werden kann. Beim Verketten von Strings können Sie daher auch auf jeden einzelnen Buchstaben zugreifen oder ihn verändern.

Eine weitere wichtige Methode zum Verketten von Strings in TypeScript ist die `concat()` Methode. Diese Methode gibt eine neue Zeichenfolge zurück, die aus der Kombination von zwei oder mehr angegebenen Zeichenfolgen besteht. Beispiel:

```TypeScript
let string1 = "Hallo";
let string2 = "Welt";
let combinedString = string1.concat(" ", string2);
console.log(combinedString);
```

Das Ergebnis dieses Codes wird ebenfalls `Hallo Welt` sein.

## Siehe auch

- [TypeScript Strings Dokumentation](https://www.typescriptlang.org/docs/handbook/strings.html)
- [MDN String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)