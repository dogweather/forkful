---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:57.584565-07:00
description: "Stringinterpolatie is een manier om variabelen rechtstreeks in een string\
  \ in te sluiten. Programmeurs gebruiken het om variabelen en strings effici\xEB\
  nt te\u2026"
lastmod: '2024-03-13T22:44:51.186094-06:00'
model: gpt-4-0125-preview
summary: "Stringinterpolatie is een manier om variabelen rechtstreeks in een string\
  \ in te sluiten. Programmeurs gebruiken het om variabelen en strings effici\xEB\
  nt te\u2026"
title: Een string interpoleren
---

{{< edit_this_page >}}

## Wat & Waarom?

Stringinterpolatie is een manier om variabelen rechtstreeks in een string in te sluiten. Programmeurs gebruiken het om variabelen en strings efficiënt te verbinden, waardoor code makkelijker te lezen en te onderhouden is.

## Hoe:

In JavaScript wordt stringinterpolatie vaak gedaan met behulp van template literals. Zo kun je het doen:

```javascript
const name = 'Alice';
const message = `Hallo, ${name}! Hoe gaat het vandaag met je?`;
console.log(message); // Geeft uit: Hallo, Alice! Hoe gaat het vandaag met je?
```

Je kunt ook operaties uitvoeren binnen placeholders:

```javascript
const a = 10;
const b = 5;
console.log(`Tien keer vijf is ${a * b}.`); // Geeft uit: Tien keer vijf is 50.
```

## Diepere Duik

Historisch gezien was stringinterpolatie niet zo eenvoudig in JavaScript. Voor ES6 (ECMAScript 2015) werd concatenatie vaak gedaan met behulp van de `+` operator:

```javascript
var name = 'Bob';
var message = 'Hallo, ' + name + '! Hoe gaat het vandaag met je?';
```

Met de introductie van ES6 kwamen template literals (tussen backticks \` \`) naar voren, met een eenvoudigere syntax dankzij de `${}` placeholders.

Alternatieven voor stringinterpolatie omvatten stringconcatenatie met de `+` operator en de `concat()` methode, of het gebruik van `sprintf`-achtige functies uit bibliotheken van derden.

De prestaties van template literals zijn over het algemeen vergelijkbaar met deze oudere methoden. Echter, de leesbaarheid en het vermogen om uitdrukkingen (zoals `${a * b}`) binnen strings op te nemen, maken template literals een sterke keuze voor ontwikkelaars.

## Zie Ook

- MDN over Template Literals: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Stringconcatenatie in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators
- Een geschiedenis van het JavaScript-module "ECMAScript": https://www.ecma-international.org/publications-and-standards/standards/ecma-262/
