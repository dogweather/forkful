---
date: 2024-01-20 17:46:05.459832-07:00
description: "Das Extrahieren von Teilzeichenketten bedeutet, bestimmte Teile einer\
  \ Zeichenkette (String) herauszuziehen. Programmierer tun das, um mit spezifischen\u2026"
lastmod: 2024-02-19 22:05:13.192378
model: gpt-4-1106-preview
summary: "Das Extrahieren von Teilzeichenketten bedeutet, bestimmte Teile einer Zeichenkette\
  \ (String) herauszuziehen. Programmierer tun das, um mit spezifischen\u2026"
title: Teilstrings extrahieren
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilzeichenketten bedeutet, bestimmte Teile einer Zeichenkette (String) herauszuziehen. Programmierer tun das, um mit spezifischen Datenfragmenten zu arbeiten, zum Beispiel beim Analysieren von Texten oder beim Manipulieren von Nutzereingaben.

## So geht’s:
```Javascript
let text = "Hallo, Welt! Willkommen zur Programmierung.";
let begruessung = text.substring(0, 5); // "Hallo"
let statement = text.slice(-24, -1); // "Willkommen zur Programmierun"

console.log(begruessung); // Gibt "Hallo" aus.
console.log(statement); // Gibt "Willkommen zur Programmierun" aus.
```

## Tiefgang:
Das Extrahieren von Teilzeichenketten in JavaScript ist keine neue Erfindung. Funktionen wie `substring()`, `slice()` und `substr()` (veraltet) sind schon lange Zeit im Spiel. 

- `substring(start, end)`: Gibt einen Teilstring zwischen Start- und Endindex (exklusiv Endindex) zurück. Negative Indizes werden als 0 interpretiert.
- `slice(start, end)`: Ähnlich wie `substring`, aber unterstützt negative Indizes, die vom Ende der Zeichenkette zählen.
- `substr(start, length)`: Historisch genutzt, um ab einem Startindex eine bestimmte Anzahl von Zeichen zu extrahieren. Es ist jedoch in modernem JavaScript als veraltet markiert.

Bei der Wahl zwischen diesen Methoden hängt es vom Kontext ab: `slice()` bietet mehr Flexibilität durch negative Indizes, während `substring()` einfacher bei positiven Indexoperationen ist. Da `substr()` veraltet ist, sollte man es meiden.

## Siehe Auch:
- MDN Web Docs zu `String.prototype.substring()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- MDN Web Docs zu `String.prototype.slice()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- Diskussion über `substr()` Veraltung: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr
