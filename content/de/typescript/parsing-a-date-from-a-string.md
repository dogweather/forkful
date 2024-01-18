---
title:                "Ein Datum aus einem String extrahieren"
html_title:           "TypeScript: Ein Datum aus einem String extrahieren"
simple_title:         "Ein Datum aus einem String extrahieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Parsen eines Datums aus einer Zeichenkette bedeutet, aus einer Textdarstellung eines Datums eine tatsächliche Datumsangabe zu extrahieren. Programmierer tun dies, um mit Datumsangaben in ihrem Code arbeiten zu können, da diese in Textform nicht verarbeitet werden können.

# Wie geht's?
In TypeScript kann ein Datum aus einer Zeichenkette mithilfe der Date-Klasse und der integrierten Funktion 'parse' geparst werden.
```
let dateString: string = "01/01/2020";
let date: Date = Date.parse(dateString);
console.log(date); // Output: 1577836800000
```
Dieser Output gibt die Anzahl der Millisekunden seit dem 1. Januar 1970 zurück. Um eine lesbarere Ausgabe zu erhalten, kann die 'toLocaleDateString' Funktion verwendet werden.
```
let dateString: string = "01/01/2020";
let date: Date = Date.parse(dateString);
console.log(date.toLocaleDateString()); // Output: 01.01.2020
```
Es ist auch möglich, das Format des Datums in der Ausgabe anzupassen, indem das zweite Argument der 'toLocaleDateString' Funktion verwendet wird.
```
let dateString: string = "01/01/2020";
let date: Date = Date.parse(dateString);
console.log(date.toLocaleDateString('de-DE', { dateStyle: 'short' })); // Output: 1.1.20
```

# Deep Dive
Das Parsen von Datumsangaben aus Zeichenketten ist ein häufiges Problem in der Programmierung. In älteren Programmiersprachen mussten Entwickler oft komplexe Algorithmen schreiben, um ein Datum aus einer Zeichenkette zu extrahieren. Heutzutage bieten viele Sprachen, einschließlich TypeScript, integrierte Funktionen oder Bibliotheken zur Vereinfachung dieses Prozesses an. Alternativ kann auch eine externe Bibliothek wie moment.js verwendet werden.

# Siehe auch
- [Date Klasse in TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#date) 
- [Moment.js Bibliothek](https://momentjs.com/)