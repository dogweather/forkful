---
title:                "Großschreibung eines Strings"
html_title:           "TypeScript: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Oft müssen wir mit Zeichenketten arbeiten, die nicht immer in der richtigen Schreibweise sind. Durch das Großschreiben der ersten Buchstaben können wir sicherstellen, dass unsere Zeichenketten lesbarer und professioneller aussehen.

## So geht's

Um eine Zeichenkette in TypeScript zu kapitalisieren, gibt es einige verschiedene Möglichkeiten.

### Variante 1: Mit der toUpperCase() Methode

Wir können die toUpperCase() Methode verwenden, um die gesamte Zeichenkette in Großbuchstaben zu verwandeln. Das sieht so aus:

```TypeScript
let str: string = "hallo welt";
console.log(str.toUpperCase()); // Ausgabe: HALLO WELT
```

### Variante 2: Mit der substr() und toUpperCase() Methode

Eine andere Möglichkeit ist es, die substr() und toUpperCase() Methode zu kombinieren, um nur den ersten Buchstaben in eine Großbuchstabe zu verwandeln. Hier ist ein Beispiel:

```TypeScript
let str: string = "hallo welt";
let capitalizedString: string = str.substr(0, 1).toUpperCase() + str.substr(1);
console.log(capitalizedString); // Ausgabe: Hallo Welt
```

## Deep Dive

Wenn wir uns die beiden Varianten genauer ansehen, können wir feststellen, dass die zweite Variante etwas komplexer ist. Jedoch bietet sie auch mehr Flexibilität, da wir damit z.B. auch die ersten Buchstaben von Wörtern in einer Zeichenkette großschreiben können.

Wir können auch eine Funktion erstellen, die diese Logik für uns kapselt und die erste Buchstabe jeder Zeichenkette großschreibt:

```TypeScript
function capitalize(str: string): string {
  return str.substr(0, 1).toUpperCase() + str.substr(1);
}

console.log(capitalize("guten morgen")); // Ausgabe: Guten Morgen
```

## Siehe auch

- [String Manipulation in TypeScript](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-manipulation-types) 
- [TypeScript Playground](https://www.typescriptlang.org/play)