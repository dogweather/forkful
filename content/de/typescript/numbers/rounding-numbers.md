---
date: 2024-01-26 03:46:54.448527-07:00
description: "Wie: Das Runden in TypeScript kann mit mehreren Methoden durchgef\xFC\
  hrt werden. Hier ist eine schnelle Durchf\xFChrung."
lastmod: '2024-03-13T22:44:53.626426-06:00'
model: gpt-4-0125-preview
summary: "Das Runden in TypeScript kann mit mehreren Methoden durchgef\xFChrt werden."
title: Zahlen runden
weight: 13
---

## Wie:
Das Runden in TypeScript kann mit mehreren Methoden durchgeführt werden. Hier ist eine schnelle Durchführung:

```typescript
// Math.round rundet zur nächsten Ganzzahl
console.log(Math.round(1.5)); // Ausgabe: 2

// Math.ceil rundet auf zur nächsten Ganzzahl
console.log(Math.ceil(1.1)); // Ausgabe: 2

// Math.floor rundet ab zur nächsten Ganzzahl
console.log(Math.floor(1.8)); // Ausgabe: 1

// toFixed rundet auf eine feste Anzahl von Dezimalstellen
let num = 1.23456;
console.log(num.toFixed(2)); // Ausgabe: "1.23"
// Hinweis: toFixed gibt eine Zeichenkette zurück! Benutze parseFloat, um zurück zu konvertieren, falls nötig.
console.log(parseFloat(num.toFixed(2))); // Ausgabe: 1.23
```

## Tiefergehend
Früher war das Runden ein Muss aufgrund von begrenztem Speicherplatz und Präzisionsproblemen in den ersten Computern. Heute kann die Fließkomma-Arithmetik aufgrund der Art und Weise, wie Zahlen binär gespeichert werden, zu skurrilen Ergebnissen führen. Alternativen zum Runden beinhalten floor, ceil und trunc (zum Abschneiden von Dezimalstellen ohne Rundung).

Die Interna sind beachtenswert: `Math.round` folgt dem "round half up" (auch bekannt als "kommerzielles Runden"), während `Math.floor` und `Math.ceil` unkompliziert sind. `toFixed` kann unerwartete Ergebnisse verursachen, weil es eine Zeichenkette zurückgibt und mit "round half to even" (auch bekannt als "Bankers Rounding") rundet, was besonders nützlich ist, um eine Verzerrung beim mehrmaligen Runden derselben Zahlen zu reduzieren.

## Siehe auch
- [MDN - Math.round()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE-Standard für Gleitkomma-Arithmetik (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
