---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (Regular Expressions oder RegEx) sind Muster zur Abgleichung und Manipulierung von Textstrings. Sie sind nützlich, um Muster in Daten zu suchen, zu verifizieren oder zu trennen und sie dann entsprechend zu verarbeiten.

## So geht's:

Hier ist ein einfacher RegEx in TypeScript zum Überprüfen einer E-Mail-Adresse.

```TypeScript
let regex: RegExp = new RegExp('^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$');
let testEmail: string = 'muster@beispiel.de';
console.log(regex.test(testEmail)); // gibt true zurück
```

Du kannst auch einen RegEx verwenden, um Text in einem String zu suchen und zu ersetzen.

```TypeScript
let s: string = 'Hallo Welt!';
s = s.replace(/Welt/g, 'TypeScript');
console.log(s); // gibt 'Hallo TypeScript!' zurück
```

## Tiefgang:

RegEx gibt es schon seit den 1950er Jahren und wurde seitdem in vielen Programmiersprachen implementiert. Sie sind ausgesprochen mächtig, haben aber auch ihre Grenzen und werden oft als schwer lesbar kritisiert.

Alternativen zu RegEx könnten spezifische String-Methoden in TypeScript sein wie `indexOf()`, `startsWith()`, `endsWith()`, usw. Sie sind weniger mächtig, aber leichter zu lesen und zu verstehen.

Obwohl TypeScript eine statisch typisierte Superset von JavaScript ist, sind die RegEx-Implementierungen zwischen den beiden Sprachen gleich. Der Unterschied liegt in der Möglichkeit, den RegEx-Typ in TypeScript explizit zu deklarieren.

## Siehe auch:

-   [RegEx Dokumentation in Mozilla Developer Network (MDN)](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)

-   [TypeScript Handbook: RegEx](https://www.typescriptlang.org/docs/handbook/2/objects.html#array-regex) 

-   [JavaScript.info: RegEx](https://javascript.info/regular-expressions)