---
title:    "Javascript: Untersuchen von Teilstrings"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine nützliche Fähigkeit beim Programmieren in JavaScript. Es ermöglicht uns, bestimmte Teile eines Strings zu isolieren und damit einfacher zu manipulieren oder zu vergleichen.

## Wie Geht's

Um einen Teilstring in JavaScript zu extrahieren, verwenden wir die `substring()` Methode. Diese Methode erwartet zwei Parameter: den Startindex und den Endindex des Substrings. Hier ist ein Beispiel:

```
let string = "Hallo Welt";
let substring = string.substring(6, 10);
console.log(substring); // Ausgabe: "Welt"
```

In diesem Beispiel haben wir den Teilstring "Welt" extrahiert, indem wir den Startindex auf 6 (das erste "W" im String) und den Endindex auf 10 (das "t" im String) gesetzt haben. Beachten Sie, dass der Endindex nicht in den Teilstring eingeschlossen wird.

Sie können auch den Startindex weglassen, in diesem Fall extrahiert die Methode den Teilstring ab dem Startindex bis zum Ende des Strings:

```
let substring = string.substring(6);
console.log(substring); // Ausgabe: "Welt"
```

Durch das Weglassen des Endindex wird der Teilstring ab dem Startindex bis zum Ende des Strings extrahiert.

```
let substring = string.substring(0, 5);
console.log(substring); // Ausgabe: "Hallo"
```

Es ist auch möglich, negative Indizes zu verwenden. In diesem Fall zählen wir von hinten, beginnend mit -1. Der letzte Index eines Strings wäre also -1, der vorletzte -2 usw.

```
console.log(string.substring(-5)); // Ausgabe: "Welt"
console.log(string.substring(-8, -6)); // Ausgabe: "Ha"
```

## Tiefere Einblicke

Eine Sache, auf die man achten sollte, ist, dass die `substring()` Methode immer einen neuen String zurückgibt. Dies bedeutet, dass der ursprüngliche String nicht verändert wird, auch wenn Sie eine Variable dafür verwenden.

```
let string = "Hallo Welt";
let substring = string.substring(6);
console.log(string); // Ausgabe: "Hallo Welt"
console.log(substring); // Ausgabe: "Welt"
```

Die `substring()` Methode ist auch nicht die einzige Möglichkeit, Teilstrings in JavaScript zu extrahieren. Die `slice()` Methode funktioniert ähnlich wie `substring()`, verwendet aber negative Indizes etwas anders und ermöglicht es uns, auch den Endindex einzuschließen.

```
console.log(string.slice(6)); // Ausgabe: "Welt"
console.log(string.slice(-5)); // Ausgabe: "Welt"
console.log(string.slice(6, 10)); // Ausgabe: "Welt"
console.log(string.slice(6, -1)); // Ausgabe: "Wel"
```

## Siehe Auch

- [MDN documentation on substring](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN documentation on slice](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Difference between substring and slice in JavaScript](https://dev.to/abdisalan_js/substring-vs-slice-in-javascript-dilemma-29og)