---
date: 2024-01-20 17:51:14.346851-07:00
description: "Interpolering av str\xE4ngar l\xE5ter dig infoga variabler eller uttryck\
  \ direkt i en str\xE4ng. Det f\xF6renklar och g\xF6r koden mer l\xE4sbar n\xE4r\
  \ du beh\xF6ver bygga\u2026"
lastmod: '2024-03-13T22:44:38.279570-06:00'
model: gpt-4-1106-preview
summary: "Interpolering av str\xE4ngar l\xE5ter dig infoga variabler eller uttryck\
  \ direkt i en str\xE4ng. Det f\xF6renklar och g\xF6r koden mer l\xE4sbar n\xE4r\
  \ du beh\xF6ver bygga\u2026"
title: "Interpolera en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av strängar låter dig infoga variabler eller uttryck direkt i en sträng. Det förenklar och gör koden mer läsbar när du behöver bygga strängar dynamiskt.

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
