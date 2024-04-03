---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:00.948515-07:00
description: "Code organiseren in functies verdeelt taken in herbruikbare stukken,\
  \ waardoor de code schoner en beter onderhoudbaar wordt. Dit doen we om redundantie\
  \ te\u2026"
lastmod: '2024-03-13T22:44:51.207411-06:00'
model: gpt-4-0125-preview
summary: Code organiseren in functies verdeelt taken in herbruikbare stukken, waardoor
  de code schoner en beter onderhoudbaar wordt.
title: Code organiseren in functies
weight: 18
---

## Hoe te:
```javascript
// Definieer een functie om de oppervlakte van een rechthoek te berekenen
function calculateArea(width, height) {
  return width * height;
}

// Roep de functie aan en print het resultaat
let area = calculateArea(5, 3);
console.log(area); // Uitvoer: 15
```

```javascript
// Groepeer gerelateerde functionaliteit met behulp van functies
function greet(name) {
  console.log(`Hallo, ${name}!`);
}

function farewell(name) {
  console.log(`Vaarwel, ${name}!`);
}

greet('Alice'); // Uitvoer: Hallo, Alice!
farewell('Bob'); // Uitvoer: Vaarwel, Bob!
```

## Diepgaand
Historisch gezien misten imperatieve programmeertalen zoals vroege versies van BASIC of Assembly de abstractie die functies bieden. In de loop van de tijd introduceerde het concept van modulaire code in talen zoals C het idee dat het opsplitsen van code in eenheden (functies of procedures) leidt tot een betere organisatie en duidelijkere logica.

In JavaScript hebben we, naast gewone functies, sinds ES6 (2015) pijlfuncties die een bondigere syntaxis bieden en geschikt zijn voor niet-methoden functies.

Alternatieven en verbeteringen rond het organiseren van code in JavaScript omvatten objectgeoriënteerde benaderingen met behulp van klassen, of functionele programmeerparadigma's die functies behandelen als eersterangsburgers.

Qua implementatie ondersteunen JavaScript-functies closures, wat een manier biedt om toegang te behouden tot de scope van een functie na uitvoering, wat krachtig is voor inkapseling en het creëren van fabrieksfuncties, onder andere patronen.

## Zie Ook
- MDN Web Docs over Functies: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- JavaScript Design Patterns: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Schone Code JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
