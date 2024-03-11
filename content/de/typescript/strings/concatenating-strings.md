---
date: 2024-01-20 17:35:48.610543-07:00
description: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren\
  \ Textst\xFCcken. Das ist n\xF6tig, um dynamische Texte zu erstellen oder unterschiedliche\u2026"
lastmod: '2024-03-11T00:14:27.513156-06:00'
model: gpt-4-1106-preview
summary: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren Textst\xFC\
  cken. Das ist n\xF6tig, um dynamische Texte zu erstellen oder unterschiedliche\u2026"
title: "Zeichenketten verkn\xFCpfen"
---

{{< edit_this_page >}}

## Was & Warum?
String-Konkatenation ist das Zusammenfügen von zwei oder mehreren Textstücken. Das ist nötig, um dynamische Texte zu erstellen oder unterschiedliche Datenquellen sinnvoll in einem String zu kombinieren.

## How to:
Mit TypeScript geht das so:

```TypeScript
let gruss: string = "Hallo";
let ort: string = "Welt";
let kompletterGruss: string = gruss + ", " + ort + "!"; // Klassische Konkatenation

console.log(kompletterGruss); // "Hallo, Welt!"

// Mit Template Strings (ES6+)
let bessererGruss: string = `${gruss}, ${ort}!`;

console.log(bessererGruss); // "Hallo, Welt!"
```

## Deep Dive
In den frühen Tagen von JavaScript war `+` der Weg, um Strings zu verketten. Mit ES6 kamen Template Strings, die mit Backticks `` ` `` geschrieben werden und `${}` für Ausdrücke in Strings. Das macht den Code lesbarer und verhindert viele Fehler, die bei der klassischen Konkatenation auftreten können.

Alternativ gibt es Methoden wie `concat()`, die aber seltener genutzt werden, da sie umständlicher sind:

```TypeScript
let kompletterGrussAlt: string = gruss.concat(", ", ort, "!");
```

Beachte: Bei großen Datenmengen kann die Performance von Konkatenationsmethoden relevant sein. Moderne JavaScript-Engines optimieren jedoch oft selbstständig.

## See Also
- [Template Literals (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [String.prototype.concat() (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
