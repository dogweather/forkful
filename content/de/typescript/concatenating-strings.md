---
title:                "TypeScript: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum 

Das Verketten von Zeichenketten ist eine grundlegende Funktion in der TypeScript-Programmierung. Es ermöglicht uns, mehrere Zeichenketten zu einem einzigen String zu kombinieren, der dann weiterverarbeitet werden kann.

## Wie man Zeichenketten verketten kann

Das Verketten von Zeichenketten ist in TypeScript sehr einfach. Wir verwenden einfach den "+" Operator, um zwei oder mehr Zeichenketten zusammenzufügen. Schauen wir uns hier ein Beispiel an:

```TypeScript 
let name: string = "Max";
let greeting: string = "Hallo, ";

let fullName: string = greeting + name;

console.log(fullName); // Output: "Hallo, Max"
```

In diesem Beispiel haben wir zwei Variablen, "name" und "greeting", die beide vom Typ "string" sind. Wir verwenden dann den "+" Operator, um die beiden Zeichenketten zusammenzufügen und in der Variable "fullName" zu speichern. Wenn wir den "fullName" String ausgeben, erhalten wir "Hallo, Max".

## Eine tiefere Betrachtung der Zeichenketten-Verkettung

Bei der Zeichenketten-Verkettung sollte beachtet werden, dass der "+" Operator nicht nur für Strings funktioniert, sondern auch für andere Datentypen wie Zahlen oder Booleans. Allerdings sollte man vorsichtig sein, da dies zu unerwarteten Ergebnissen führen kann. Hier ein weiteres Beispiel:

```TypeScript
let num1: number = 5;
let num2: number = 10;

let result: string = "Das Ergebnis ist " + num1 + num2;
console.log(result); // Output: "Das Ergebnis ist 510"
```

In diesem Beispiel haben wir Zahlen anstatt Strings verwendet. Das Ergebnis ist jedoch immer noch ein String, da der "+" Operator automatisch die Zahlen in Strings umwandelt. Deshalb werden die beiden Zahlen nicht addiert, sondern aneinandergehängt.

## Siehe auch

- <https://www.typescriptlang.org/docs/handbook/basic-types.html#string>
- <https://www.freecodecamp.org/news/the-typescript-handbook/>
- <https://www.educba.com/typescript-string/>