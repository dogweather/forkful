---
title:                "Einsatz von regulären Ausdrücken"
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# TypeScript und Reguläre Ausdrücke: Ein knackiger Guide

## Was & Warum?
Reguläre Ausdrücke sind Muster, die Strings nach bestimmten Regeln matchen. Sie sind unverzichtbar für Suche, Validierung und Manipulation von Text in der Programmierung.

## How to:
```TypeScript
const text: string = "Programmieren mit TypeScript ist großartig!";
const regex: RegExp = /TypeScript/;

// Prüfen, ob das Muster im String vorkommt
console.log(regex.test(text)); // Ausgabe: true

// Text ersetzen
const newText: string = text.replace(regex, "JavaScript");
console.log(newText); // Ausgabe: "Programmieren mit JavaScript ist großartig!"

// Extraktion eines Patterns
const emailRegex: RegExp = /(\S+@\S+\.\S+)/;
const sampleText: string = "Meine Email ist beispiel@domain.de im Text.";
const emailMatch = sampleText.match(emailRegex);

console.log(emailMatch ? emailMatch[0] : "Keine E-Mail gefunden"); // Ausgabe: beispiel@domain.de
```

## Deep Dive
Reguläre Ausdrücke, kurz RegEx, entstanden in den 1950er Jahren und wurden von dem Mathematiker Stephen Cole Kleene entwickelt. Alternativen zu RegEx sind spezialisierte Parser für komplexe Syntax, doch RegEx bleiben die erste Wahl für einfache Mustererkennung. Wichtig: RegEx in TypeScript werden durch das JavaScript-RegEx-Engine implementiert, da TypeScript zu JavaScript kompiliert wird.

## See Also
- [MDN Reguläre Ausdrücke](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [RegEx Tester und Debugger](https://regex101.com/)