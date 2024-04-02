---
date: 2024-01-20 17:51:43.090036-07:00
description: "String-Interpolation erm\xF6glicht das Einf\xFCgen von Variablenwerten\
  \ in einen String. Das macht den Code lesbarer und die String-Manipulation effizienter."
lastmod: '2024-03-13T22:44:53.618026-06:00'
model: gpt-4-1106-preview
summary: "String-Interpolation erm\xF6glicht das Einf\xFCgen von Variablenwerten in\
  \ einen String. Das macht den Code lesbarer und die String-Manipulation effizienter."
title: Zeichenketten interpolieren
weight: 8
---

## What & Why?
String-Interpolation ermöglicht das Einfügen von Variablenwerten in einen String. Das macht den Code lesbarer und die String-Manipulation effizienter.

## How to:

In TypeScript kannst du mit Template Literals arbeiten, um Interpolation zu nutzen. Sie werden mit Backticks (`) gekennzeichnet und können Platzhalter, die `${expression}` Struktur nutzen, enthalten.

```typescript
let user = "Mona";
let greeting = `Hallo ${user}, herzlich willkommen!`;
console.log(greeting);  // "Hallo Mona, herzlich willkommen!"
```

Mit Template Literals kannst du auch mehrzeilige Strings einfach erstellen:

```typescript
let item = "Kaffee";
let price = 2.99;
let multiLineString = `Artikel: ${item}
Preis: ${price}€
Guten Einkauf!`;

console.log(multiLineString);
/* 
Ausgabe:
Artikel: Kaffee
Preis: 2.99€
Guten Einkauf!
*/
```

## Deep Dive

String-Interpolation in TypeScript bedient sich der Template Literals, die mit ES6 eingeführt wurden. Vor ES6 waren Konkatenation mit dem `+` Operator Standard, oft unübersichtlich bei mehreren Variablen.

Alternativ zur Interpolation können auch Template-Funktionen genutzt werden, meist für komplexere Szenarien oder Lokalisierung. Sie nehmen einen Template String und geben eine Funktion zurück, die die Daten interpoliert:

```typescript
function templ(strings, ...keys) {
  return (function(...values) {
    let result = [strings[0]];
    keys.forEach((key, i) => {
      result.push(values[key], strings[i + 1]);
    });
    return result.join('');
  });
}

const template = templ`Hallo ${0}, du hast ${1} neue Nachrichten.`;
console.log(template("Mona", 5));  // "Hallo Mona, du hast 5 neue Nachrichten."
```

Durch die Verwendung von Template-Funktionen können auch komplexe Logiken bei der Interpolation stattfinden, was durch einfache Template Literals nicht möglich wäre.

## See Also

- TypeScript Dokumentation zu Literal Types: [https://www.typescriptlang.org/docs/handbook/literal-types.html](https://www.typescriptlang.org/docs/handbook/literal-types.html)
- MDN-Dokumentation zu Template Literals: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- Weitere Infos zu Tagged Template Literals: [https://www.typescriptlang.org/docs/handbook/advanced-types.html#tagged-template-literals](https://www.typescriptlang.org/docs/handbook/advanced-types.html#tagged-template-literals)
