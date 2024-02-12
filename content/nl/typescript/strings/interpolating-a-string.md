---
title:                "Een string interpoleren"
date:                  2024-01-28T22:02:30.944409-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

String interpolatie stelt je in staat om variabelen en expressies in strings in te voegen. Dit houdt je code leesbaar en flexibel - geen plus tekens, geen gedoe.

## Hoe te:

Om een string te interpoleren in TypeScript, gebruik je backticks `` ` `` en `${expressie}` syntax:

```TypeScript
let user = 'Charlie';
let age = 27;

// Een string interpoleren
let groet = `Hi, ik ben ${user} en ik ben ${age} jaar oud.`;

console.log(groet);  // Uitvoer: Hi, ik ben Charlie en ik ben 27 jaar oud.
```

## Diepere Duik:

String interpolatie is niet uniek voor TypeScript; het zit ook in JavaScript sinds ES6 en vele andere talen. Voorheen concateneerden we strings met behulp van de `+` operator, wat er zo uitzag:

```TypeScript
let groet = 'Hi, ik ben ' + user + ' en ik ben ' + age + ' jaar oud.';
```

De `+` methode werkt, maar het is omslachtiger en moeilijker te lezen, vooral met meerdere variabelen. Met interpolatie zijn sjablonen schoner en fouten makkelijker te vermijden.

Wat gebeurt er onder de motorkap? Geïnterpoleerde strings zijn "syntactische suiker" - een vereenvoudigde manier om de complexere functie bekend als "template literals" te gebruiken. Wanneer gecompileerd, wordt je vriendelijke, leesbare interpolatie omgezet naar een formaat dat de JavaScript-engine kan begrijpen, vaak met behulp van concatenatie of andere string manipulatie methoden.

Een alternatief voor interpolatie zou het gebruik van sjabloonfuncties of bibliotheken kunnen zijn, maar voor de meeste gevallen is interpolatie met backticks het handigste gereedschap voor de klus.

## Zie ook:

- [Mozilla Developer Network over Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Documentatie](https://www.typescriptlang.org/docs/)
- [ES6 Functies en Syntax](http://es6-features.org/#StringInterpolation)
