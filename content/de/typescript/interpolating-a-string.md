---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stichwortartige Formate in TypeScript: Eine unkomplizierte Anleitung

## Was & Warum?

Stichwortartige Formatierungen, die in TypeScript als Template Strings bekannt sind, ermöglichen es Programmierern, Variablen in Zeichenketten einzufügen. Sie werden verwendet, um den Code lesbarer zu machen und String-Konkatenationen zu vereinfachen.

## So funktioniert's:

In TypeScript verwenden wir Backticks (`` ` ``) anstatt Anführungszeichen, um Template Strings zu definieren. Hier sind einige Beispiele:

```TypeScript
let name = "Friedrich";
let greeting = `Hallo, ${name}!`;
console.log(greeting);  // Ausgabe: "Hallo, Friedrich!"
```

Sehen wir uns einen komplexeren Fall an:

```TypeScript
let x = 5;
let y = 10;
let result = `Das Produkt von ${x} und ${y} ist ${x*y}.`;
console.log(result);  // Ausgabe: "Das Produkt von 5 und 10 ist 50."
```

## Tiefgehende Information:

- **Historischer Kontext:** Template Strings wurden mit ES6 (ECMAScript 2015) eingeführt, der sechsten Ausgabe der ECMAScript-Spezifikation, die als Grundlage für JavaScript dient.
  
- **Alternativen:** Vor der Einführung des Template-Strings mussten Programmierer die alte '+'-Methode zur Konkatenation verwenden: `let greeting = "Hallo, " + name + "!";`

- **Implementierungsdetails:** Intern verwendet JavaScript die Funktion `.toString()` um die Ausdrücke innerhalb der geschweiften Klammern (`${expression}`) zu evaluieren.

## Siehe auch:

- [Mozilla Developer Network (MDN)Template Strings Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/template_strings)
  
- [TypeScript Handbuch über String Interpolation](https://www.typescriptlang.org/docs/handbook/2/template-literals.html)

Mit Template Strings und ihrer interpolation in TypeScript können Sie Ihren Code effizienter und einfacher lesbar gestalten. Fangen Sie an, sie noch heute in Ihre Codebasis zu integrieren!