---
date: 2024-01-20 17:46:55.223349-07:00
description: "Teilstrings extrahieren bedeutet, bestimmte Abschnitte aus einem l\xE4\
  ngeren String herauszuschneiden. Programmierer machen das, um Daten zu manipulieren,\
  \ zu\u2026"
lastmod: '2024-03-11T00:14:27.509815-06:00'
model: gpt-4-1106-preview
summary: "Teilstrings extrahieren bedeutet, bestimmte Abschnitte aus einem l\xE4ngeren\
  \ String herauszuschneiden. Programmierer machen das, um Daten zu manipulieren,\
  \ zu\u2026"
title: Teilstrings extrahieren
---

{{< edit_this_page >}}

## What & Why?
Teilstrings extrahieren bedeutet, bestimmte Abschnitte aus einem längeren String herauszuschneiden. Programmierer machen das, um Daten zu manipulieren, zu validieren oder Features wie Suchen und Ersetzen zu implementieren.

## How to:
TypeScript bietet mehrere Methoden, um Teilstrings zu extrahieren: `slice()`, `substring()`, und `substr()` (letztere ist veraltet). Hier sehen wir, wie man sie anwendet:

```typescript
let text: string = "Hallo Welt! Wie geht's?";

// slice() - extrahiert einen Teilstring nach Start- und Endindex
let sliceResult: string = text.slice(0, 5);
console.log(sliceResult); // Output: Hallo

// substring() - ähnlich wie slice(), aber kann keine negativen Indizes verwenden
let substringResult: string = text.substring(7, 12);
console.log(substringResult); // Output: Welt!

// substr() - verwendet Startindex und Länge (Achtung: ist veraltet!)
let substrResult: string = text.substr(0, 5);
console.log(substrResult); // Output: Hallo
```

## Deep Dive
Teilstrings zu extrahieren, kommt aus der Notwendigkeit, mit textbasierten Daten zu arbeiten. Früher, in Sprachen wie C, musste man dafür mit Pointern hantieren. Modernere Sprachen wie TypeScript abstrahieren diese Komplexität.

`slice()` und `substring()` sind sich sehr ähnlich, aber `slice()` kann negative Indizes nehmen, um vom Ende zu zählen, während `substring()` die Indizes austauscht, wenn der Startindex größer als der Endindex ist. `substr()` ist wegen seiner Verwirrungspotentiale und Inkonsistenz der Parameterdefinition im ECMAScript-Standard als veraltet markiert und sollte vermieden werden.

Für komplexere Szenarien kann `match()` oder `RegExp` eingesetzt werden, um Substrings basierend auf Mustern zu extrahieren.

## See Also
- MDN Web Docs: Strings und String-Methoden in JavaScript/TypeScript https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String 
- TypeScript Handbook: Basic Types https://www.typescriptlang.org/docs/handbook/basic-types.html
- ECMAScript 2022 Language Specification: https://tc39.es/ecma262/ 

Beachte, dass TypeScript-Dokumentation und Ressourcen oft auf Englisch sind, daher können auch englischsprachige Quellen helfen, um ein tieferes Verständnis zu erlangen.
