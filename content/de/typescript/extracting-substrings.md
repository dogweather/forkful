---
title:                "Unterstrings extrahieren"
html_title:           "TypeScript: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine wichtige Fähigkeit in der TypeScript-Programmierung. Es ermöglicht uns, bestimmte Teile eines Strings zu isolieren und damit effizienter mit Text-Inputs und -outputs zu arbeiten.

## Wie funktioniert es?

Das Extrahieren von Teilstrings in TypeScript ist relativ einfach und kann auf verschiedene Arten durchgeführt werden. Eine Möglichkeit ist die Verwendung der `substring()`-Methode, die ein Teilstring basierend auf einem Start- und Endindex ausgibt. Hier ist ein Beispiel:

```typescript
let str = "Hallo, dies ist ein Text.";

// Extrahiere Teilstring von Index 7 bis 12
let teilstr = str.substring(7, 12);
console.log(teilstr); // Ausgabe: "dies "
```

Wir können auch die `substr()`-Methode verwenden, die ähnlich funktioniert, aber anstatt eines Endindex die Länge des auszugebenden Teilstrings als zweiten Parameter erwartet. Hier ist ein Beispiel:

```typescript
let str = "Hallo, dies ist ein Text.";

// Extrahiere Teilstring von Index 7 mit Länge von 5 Zeichen
let teilstr = str.substr(7, 5);
console.log(teilstr); // Ausgabe: "dies "
```

Wir können auch die `slice()`-Methode verwenden, um Teilstrings basierend auf negativen Indizes auszugeben. Hier ist ein Beispiel:

```typescript
let str = "Hallo, dies ist ein Text.";

// Extrahiere Teilstring von Index -14 bis -10
let teilstr = str.slice(-14, -10);
console.log(teilstr); // Ausgabe: "dies"
```

Es gibt auch die Möglichkeit, Teilstrings mit regulären Ausdrücken zu extrahieren. Zum Beispiel können wir die `match()`-Methode verwenden, um alle Vorkommen eines bestimmten Musters in einem String zu finden und als Teilstrings auszugeben. Hier ist ein Beispiel:

```typescript
let str = "Hallo, dies ist ein Text.";

// Extrahiere alle Vorkommen von "ist"
let teilstr = str.match(/ist/g);
console.log(teilstr); // Ausgabe: ["ist", "ist"]
```

## Tiefergehende Informationen

Bei der Extraktion von Teilstrings ist es wichtig zu beachten, dass die Indizes immer nullbasiert sind. Das bedeutet, dass der erste Buchstabe in einem String den Index 0 hat und nicht 1. Außerdem gibt es Unterschiede zwischen den `substring()`-, `substr()`- und `slice()`-Methoden, wenn es um die Verwendung von negativen Indizes geht. Es ist daher hilfreich, sich mit allen drei Methoden vertraut zu machen, um das gewünschte Ergebnis zu erzielen.

Es ist auch zu beachten, dass es in TypeScript spezielle Funktionen gibt, die das Arbeiten mit Teilstrings erleichtern, wie z.B. `includes()`, `startsWith()` und `endsWith()`, die überprüfen, ob ein bestimmter Teilstring in einem anderen String enthalten ist oder ob ein String mit einem bestimmten Teilstring beginnt oder endet.

## Siehe auch

- Offizielle TypeScript-Dokumentation für die `substring()`: https://www.typescriptlang.org/docs/handbook/strings.html#substring
- Offizielle TypeScript-Dokumentation für die `substr()`: https://www.typescriptlang.org/docs/handbook/strings.html#substr
- Offizielle TypeScript-Dokumentation für die `slice()`: https://www.typescriptlang.org/docs/handbook/strings.html#slice
- Tutorial zur Verwendung von regulären Ausdrücken in TypeScript: https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm