---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings bedeutet, jeden Buchstaben in Großbuchstaben umzuwandeln. Programmierer nutzen es, um Einheitlichkeit zu erzielen, den Text hervorzuheben oder bestimmte Datenformate einzuhalten.

## So geht's:
```TypeScript
function capitalizeString(input: string): string {
    return input.toUpperCase();
}

console.log(capitalizeString("hallo welt!")); // Output: HALLO WELT!
```

## Tiefgang:
Das Großschreiben von Strings ist so alt wie die Textverarbeitung selbst. In den Anfängen der Computerei musste zwischen Groß- und Kleinschreibung unterschieden werden, da Zeichencodes wie ASCII unterschiedliche Werte für Groß- und Kleinbuchstaben hatten. Heutzutage bieten die meisten Programmiersprachen Funktionen zum Großschreiben. In TypeScript/JavaScript ist `.toUpperCase()` die direkte Methode, um diesen Effekt zu erzielen. Alternativ kann man auch reguläre Ausdrücke verwenden, um nur bestimmte Teile eines Strings zu transformieren, aber das ist komplexer und für einfache Szenarien nicht benötigt. Die Implementierung von `.toUpperCase()` in JavaScript-Engines berücksichtigt auch Lokalisierung, so dass Buchstaben in verschiedenen Sprachen korrekt großgeschrieben werden.

## Siehe auch:
- [MDN Web Docs - toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Unicode Standard](https://unicode.org/standard/standard.html)
- [ECMAScript Language Specification](https://www.ecma-international.org/ecma-262/10.0/index.html#sec-string.prototype.touppercase)
