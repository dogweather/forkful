---
title:                "Unterscheidung von Teilzeichenketten"
html_title:           "TypeScript: Unterscheidung von Teilzeichenketten"
simple_title:         "Unterscheidung von Teilzeichenketten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Extrahieren von Teilstrings handelt es sich um die Aufgabe, einen bestimmten Teil eines längeren Textstücks oder einer Zeichenkette zu isolieren. Programmierer nutzen diese Technik, um Daten zu filtern oder spezifische Informationen aus einem größeren Datensatz herauszulösen.

## Wie?

```TypeScript
const name = "Max Mustermann";

console.log(name.substring(0, 3));
// Ausgabe: Max

console.log(name.substr(4));
// Ausgabe: Mustermann
```

Die Methoden `substring()` und `substr()` sind in TypeScript verfügbar und ermöglichen das Extrahieren von Teilstrings. Die Parameter geben dabei die Startposition und die Länge des zu extrahierenden Teilstrings an. Die Methode `substring()` startet dabei an der angegebenen Position und endet vor der angegebenen Länge, während `substr()` die angegebene Länge inkludiert.

## Tiefere Einblicke

Das Extrahieren von Substrings ist eine gängige Technik in der Programmierung und wurde bereits in frühen Programmiersprachen wie BASIC und FORTRAN eingesetzt. In TypeScript gibt es auch die Methode `slice()`, die ähnlich funktioniert, jedoch negative Startpositionen erlaubt, um vom Ende des Strings aus zu extrahieren.

Als Alternative zu den genannten Methoden gibt es auch reguläre Ausdrücke (Regular Expressions), die komplexere Muster erkennen und so das Extrahieren von Substrings noch flexibler gestalten können.

## Siehe auch

- [MDN Web Docs zu substring](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs zu substr](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN Web Docs zu slice](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs zu regulären Ausdrücken](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp)