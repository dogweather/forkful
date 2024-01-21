---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:28.861845-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)

Zufallszahlen in JavaScript sind praktisch, wenn man ein wenig Unvorhersagbarkeit braucht - sei es für Spiele, Simulationen oder die Auswahl aus einer Menge. Sie bringen Dynamik und Realitätsnähe ins Spiel.

## How to: (Wie geht das?)

Um eine einfache Zufallszahl zu generieren, benutzt du `Math.random()`. Das Ergebnis ist eine Fließkommazahl zwischen 0 (inklusive) und 1 (exklusive).

```javascript
let zufallszahl = Math.random();
console.log(zufallszahl); // z.B. 0.123456789
```

Möchtest du eine ganze Zahl zwischen zwei Werten, nutze diese Funktion:

```javascript
function zufallszahlZwischen(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(zufallszahlZwischen(1, 10)); // z.B. 5
```

## Deep Dive (Hinter den Kulissen)

Historisch gesehen setzt JavaScript auf einen Pseudozufallszahlengenerator (PRNG), der deterministisch ist und nicht für kryptografische Zwecke geeignet ist. Weil `Math.random()` nicht vorhersagbar sein sollte, aber im Kern auf Algorithmen basiert, ist es kein echter Zufall.

Alternative Methoden, wie die Web Cryptography API, bieten kryptografisch sichere Zufallszahlen:

```javascript
window.crypto.getRandomValues(new Uint32Array(1))[0];
```

Bei der Implementierung ist zu beachten, dass die gleichmäßige Verteilung unter Programmierern oft missverstanden wird. Auch wenn die Werte zufällig sind, bedeutet das nicht, dass nicht zufällig gleiche Werte hintereinander auftreten können.

## See Also (Siehe auch)

- MDN Web Docs zu `Math.random()`: [MDN Math.random()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- MDN Web Docs zur Web Cryptography API: [MDN Web Cryptography API](https://developer.mozilla.org/de/docs/Web/API/Web_Crypto_API)