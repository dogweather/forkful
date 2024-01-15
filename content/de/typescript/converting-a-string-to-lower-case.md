---
title:                "String in Kleinbuchstaben umwandeln"
html_title:           "TypeScript: String in Kleinbuchstaben umwandeln"
simple_title:         "String in Kleinbuchstaben umwandeln"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum es nützlich sein kann, in der Programmierung eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Eine häufige Verwendung ist die Normalisierung von Benutzereingaben, um sicherzustellen, dass alle Zeichen in derselben Schreibweise vorliegen. Dadurch können Vergleiche und andere Operationen einfacher durchgeführt werden, ohne sich Gedanken über Groß- und Kleinschreibung zu machen.

## Wie geht das

Um eine Zeichenfolge in TypeScript in Kleinbuchstaben umzuwandeln, können wir die `toLowerCase()` Methode verwenden. Diese wird auf einer Zeichenfolge aufgerufen und gibt die Zeichenfolge in Kleinbuchstaben zurück.

```TypeScript
const name = "Max Mustermann";
const nameLowerCase = name.toLowerCase(); // "max mustermann"
```

Die `toLowerCase()` Methode ist nicht nur auf Zeichenfolgen anwendbar, sondern auch auf Array-Elemente, die Zeichenfolgen enthalten können. Dadurch können wir z. B. alle Elemente eines Arrays in Kleinbuchstaben umwandeln.

```TypeScript
const fruits = ["Apfel", "Banane", "Orange"];
const fruitsLowerCase = fruits.map(fruit => fruit.toLowerCase()); // ["apfel", "banane", "orange"]
```

## Tiefergehende Informationen

Bei der Umwandlung in Kleinbuchstaben gibt es einige wichtige Aspekte zu beachten. Zum einen kann es Unterschiede zwischen verschiedenen Sprachen geben, wie z. B. die Verwendung von Sonderzeichen oder Akzenten. Die `toLowerCase()` Methode berücksichtigt diese Unterschiede und konvertiert die Zeichenfolge entsprechend. 

Ein weiterer wichtiger Punkt ist, dass die `toLowerCase()` Methode nicht veränderlich ist. Das heißt, sie ändert nicht die ursprüngliche Zeichenfolge, sondern gibt eine neue, konvertierte Zeichenfolge zurück. Es ist daher wichtig, die Rückgabe der Methode in einer Variablen zu speichern, wenn die ursprüngliche Zeichenfolge später weiter verwendet werden soll.

## Siehe auch

- [JavaScript string `toLowerCase()` Methode](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript array `map()` Methode](https://www.typescriptlang.org/docs/handbook/2/iterable-types.html#mapping-types)