---
title:                "Ermittlung der Zeichenkettenlänge"
aliases:
- /de/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:23.395302-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings in TypeScript zu finden, bedeutet, zu ermitteln, wie viele Zeichen der String enthält. Programmierer müssen das häufig tun, um Texteingaben zu validieren, Schleifen zu steuern oder Daten zu manipulieren.

## So geht's:
Um die Länge eines Strings zu bekommen, nutzt du die `.length` Eigenschaft. Hier ein schnelles Beispiel:

```TypeScript
let greeting: string = "Hallo Welt!";
console.log(greeting.length); // Ausgabe: 11
```

## Deep Dive
Die `.length` Eigenschaft von Strings ist grundlegend in vielen Programmiersprachen und stammt aus den Anfängen der Informatik, als das Verständnis der Datenlänge für die Speicherverwaltung entscheidend war. In TypeScript gibt es keine direkt sichtbaren Alternativen zur `.length` Eigenschaft, aber im JavaScript-Umfeld könnten einige Polyfills oder String-Verarbeitungsbibliotheken ähnliche Funktionen anbieten. Intern wird die Länge eines Strings im JavaScript-Laufzeitsystem als eine einfache Eigenschaft gespeichert, daher ist der Zugriff auf `.length` sehr effizient.

## See Also
- TypeScript Handbook: [Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- MDN Web Docs über String Länge: [String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Typenmanipulation in TypeScript: [Advanced Types](https://www.typescriptlang.org/docs/handbook/advanced-types.html)
