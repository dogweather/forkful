---
title:                "Verkettung von Zeichenketten"
html_title:           "TypeScript: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Konkatenieren von Strings handelt es sich um das Zusammenfügen von mehreren Zeichenfolgen zu einer großen Zeichenfolge. Programmierer nutzen dies, um beispielsweise Text oder Variableninhalte zusammenzufügen und in einer bestimmten Reihenfolge auszugeben.

## How to:

Konkatenieren von Strings in TypeScript ist relativ einfach. Dazu können wir den Plus-Operator (+) verwenden, um zwei oder mehr Strings miteinander zu verbinden. Hier ist ein Beispiel:

```TypeScript
let name: string = "Max";
let greeting: string = "Hallo " + name + "!";

console.log(greeting); // Ausgabe: Hallo Max!
```

Wir können auch Zahlen mit Strings konkatenieren, indem wir sie zuerst in Strings umwandeln. Hier ist ein Beispiel:

```TypeScript
let num1: number = 10;
let num2: number = 20;
let result: string = "Die Summe von " + num1.toString() + " und " + num2.toString() + " ist " + (num1 + num2).toString();

console.log(result); // Ausgabe: Die Summe von 10 und 20 ist 30
```

## Deep Dive:

Der Begriff "Konkatenation" kommt aus dem Lateinischen und bedeutet "Anhängen", was den Prozess des Zusammenfügens von Zeichenfolgen gut beschreibt. Fragmente von Zeichenfolgen können auch mit dem Array-ähnlichen `join()`-Methode zusammengefügt werden. TypeScript unterstützt auch die Template-Literals-Syntax für Strings, die es ermöglicht, Werte von Variablen direkt in Strings einzufügen.

Es gibt auch andere Möglichkeiten, Strings in TypeScript zu konkatenieren, wie z.B. die `concat()`-Funktion oder die Verwendung von Template-Strings. Letztendlich hängt es aber von den persönlichen Vorlieben des Programmierers ab, welche Methode verwendet werden soll.

## See Also:

- Offizielle TypeScript-Dokumentation zur Zeichenfolgenkonkatenation: https://www.typescriptlang.org/docs/handbook/Basic Types.html#string-concatenation
- Eine Einführung in die Template-Literals in TypeScript: https://www.tutorialsteacher.com/typescript/typescript-template-literals