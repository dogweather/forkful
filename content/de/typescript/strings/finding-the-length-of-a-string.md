---
date: 2024-01-20 17:48:23.395302-07:00
description: "Die L\xE4nge eines Strings in TypeScript zu finden, bedeutet, zu ermitteln,\
  \ wie viele Zeichen der String enth\xE4lt. Programmierer m\xFCssen das h\xE4ufig\
  \ tun, um\u2026"
lastmod: '2024-03-13T22:44:53.622679-06:00'
model: gpt-4-1106-preview
summary: "Die L\xE4nge eines Strings in TypeScript zu finden, bedeutet, zu ermitteln,\
  \ wie viele Zeichen der String enth\xE4lt."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

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
