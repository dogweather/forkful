---
date: 2024-01-20 17:51:14.346851-07:00
description: "Hur g\xF6r man: \xC4ldre s\xE4tt, innan ES6."
lastmod: '2024-04-05T21:53:39.616740-06:00'
model: gpt-4-1106-preview
summary: "\xC4ldre s\xE4tt, innan ES6."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur gör man:
```javascript
// Exempel med template literals (ES6 och framåt)
let namn = 'Erik';
let meddelande = `Hej ${namn}, välkommen tillbaka!`;
console.log(meddelande); // Output: Hej Erik, välkommen tillbaka!
```

Äldre sätt, innan ES6:
```javascript
let namn = 'Erik';
let meddelande = 'Hej ' + namn + ', välkommen tillbaka!';
console.log(meddelande); // Output: Hej Erik, välkommen tillbaka!
```

## Fördjupning
Interpolering av strängar har funnits i programmeringsspråk länge, men JavaScript fick det först med ES6 (EcmaScript 2015) och användningen av *template literals*. Tidigare sätt att bygga strängar inkluderade konkatenering med `+` eller arraymetoder som `join()`.

Alternativ till interpolering:
- Konkatenering: `let fullständigtMeddelande = 'Hej ' + förnamn + ' ' + efternamn;`
- `join()`: `let delar = ['Hej', förnamn, efternamn]; let fullständigtMeddelande = delar.join(' ');`

Implementeringsdetaljer:
- Interpolering kan använda uttryck, inte bara variabler: ``let meddelande = `Resultatet är ${1 + 2}`; // "Resultatet är 3"``
- Template literals tillåter flerradiga strängar utan `\n`: ``let lista = `Första raden andra raden tredje raden`;``
- Template strings kan innehålla "tagged template literals", vilket är avancerat och låter en funktion bearbeta strängen och interpoleringar.

## Se även
- MDN Web Docs: Template literals (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- ECMAScript 2015 Language Specification: Template Literals (https://www.ecma-international.org/ecma-262/6.0/#sec-template-literals)
