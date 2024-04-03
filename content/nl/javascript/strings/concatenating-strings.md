---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:08.005216-07:00
description: "Het aan elkaar plakken van strings betekent dat je ze achter elkaar\
  \ koppelt. We doen dit om berichten, URL's of elke tekst met stukjes & beetjes uit\u2026"
lastmod: '2024-03-13T22:44:51.192514-06:00'
model: gpt-4-0125-preview
summary: Het aan elkaar plakken van strings betekent dat je ze achter elkaar koppelt.
title: Samenvoegen van strings
weight: 3
---

## Wat & Waarom?
Het aan elkaar plakken van strings betekent dat je ze achter elkaar koppelt. We doen dit om berichten, URL's of elke tekst met stukjes & beetjes uit verschillende bronnen te creëren.

## Hoe te:
In JavaScript heb je een paar manieren om strings aan elkaar te plakken. Ouderwets: `+`. Modern: sjabloonliteralen. Hier is hoe ze eruitzien.

**Gebruikmakend van de + operator:**
```javascript
let hello = "Hallo, ";
let world = "wereld!";
let groet = hello + world; 
console.log(groet); // "Hallo, wereld!"
```

**Gebruikmakend van sjabloonliteralen:**
```javascript
let gebruiker = "Jane";
let welkomstbericht = `Hoi, ${gebruiker}! Welkom terug.`;
console.log(welkomstbericht); // "Hoi, Jane! Welkom terug."
```

## Uitdieping
Vroeger was `+` de manier om te gaan, maar het werd een rommeltje met veel variabelen. Toen kwam ES6 in 2015, met de introductie van sjabloonliteralen (die backticks `\``). Dit betekende beter uitziende strings en de mogelijkheid om variabelen en uitdrukkingen direct in je string te gooien zonder te zweten.

**Waarom `+` lastig kan zijn:**
- Moeilijker te lezen met meerdere variabelen.
- Makkelijk om spaties over het hoofd te zien, wat leidt tot aan elkaar geplakte woorden.
- Plus, wie heeft al die plussen nodig?

**Waarom sjabloonliteralen geweldig zijn:**
- Leesbaarheid: Als een Engelse zin met ingevulde lege plekken.
- Ondersteuning voor meerdere regels: Je kunt strings creëren die meerdere regels beslaan zonder `+` of `\n`.
- Interpolatie van expressies: Stop variabelen erin, doe wiskunde, allemaal in één keer.

**Hier is multiline en expressies in actie:**
```javascript
let appels = 3;
let sinaasappels = 5;
let fruitSamenvatting = `Je hebt ${appels + sinaasappels} stuks fruit: 
${appels} appels en 
${sinaasappels} sinaasappels.`;
console.log(fruitSamenvatting);
```
Levert een netjes geformatteerde samenvatting op zonder enige `+` acrobatiek.

Technisch gezien creëert stringconcatenatie elke keer dat je `+` gebruikt een nieuwe string. Voor de computer is dat alsof je elke keer een hele nieuwe reep maakt wanneer je gewoon een pinda wilt toevoegen. Niet erg efficiënt. Sjabloonliteralen zijn als een mal waar je alle ingrediënten tegelijk in kunt gooien – betere prestaties, vooral bij grote strings of in lussen.

## Zie ook
- MDN Web Docs over sjabloonliteralen (voor verder lezen): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Stringmethoden en -eigenschappen (nuttig bij het omgaan met strings): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
