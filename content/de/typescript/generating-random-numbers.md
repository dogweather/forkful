---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:50:03.054328-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Was und Warum?)
Zufallszahlen sind in der Programmierung essenziell für Spiele, Simulationen und Tests. Sie erzeugen Unvorhersehbarkeit und ermöglichen die Modellierung von Zufallsereignissen.

## How to: (Wie geht das?)
TypeScript nutzt JavaScript's `Math.random()` für Zufallszahlen. Hier zwei Beispiele:

### Beispiel 1: Einfache Zufallszahl von 0 bis 1
```TypeScript
let randomNumber: number = Math.random();
console.log(randomNumber);
```
Ausgabe könnte sein: `0.4378294`

### Beispiel 2: Ganze Zufallszahl zwischen zwei Werten
```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 10));
```
Ausgabe wäre eine Zahl zwischen 1 und 10, z.B. `7`.

## Deep Dive (Tiefer Eintauchen)
In JavaScript und TypeScript geht `Math.random()` auf die 1990er zurück und ist nicht kryptographisch sicher. Für Spiele und einfache Anwendungen reicht es, aber nicht für Sicherheitsanwendungen. Dort benutzt man `crypto.getRandomValues()`. Die Funktion `Math.random()` erzeugt Pseudozufallszahlen, echte Zufälligkeit erreicht man in Computern nur schwer.

## See Also (Siehe auch)
- MDN Web Docs on Math.random(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Mozilla's documentation on `crypto.getRandomValues()`: https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
- A TypeScript playground to experiment with the code: https://www.typescriptlang.org/play
