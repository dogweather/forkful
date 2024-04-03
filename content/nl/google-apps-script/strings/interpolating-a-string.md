---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:15.789393-07:00
description: "String interpolatie in Google Apps Script maakt het mogelijk om dynamisch\
  \ expressies in te voegen binnen strings, wat helpt om meer leesbare en\u2026"
lastmod: '2024-03-13T22:44:50.316311-06:00'
model: gpt-4-0125-preview
summary: "String interpolatie in Google Apps Script maakt het mogelijk om dynamisch\
  \ expressies in te voegen binnen strings, wat helpt om meer leesbare en onderhoudbare\
  \ code te cre\xEBren."
title: Een string interpoleren
weight: 8
---

## Hoe:
In Google Apps Script wordt stringinterpolatie bereikt door middel van template literals. Dit zijn string literals die ingesloten expressies toelaten, aangeduid met backticks (\`) in plaats van de gebruikelijke aanhalingstekens. Hier is hoe je ze kunt gebruiken:

```javascript
// Een basisvoorbeeld
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Hallo, ${user}!`); // Uitvoer: Hallo, Alice!
}

// Expressies gebruiken
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Vijf plus tien is ${a + b}.`); // Uitvoer: Vijf plus tien is 15.
}

// Meerdere regels strings
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`Dit is een string over meerdere regels:
Hallo allemaal,
Vandaag bespreken we ${item}.`);
  // Uitvoer:
  // Dit is een string over meerdere regels:
  // Hallo allemaal,
  // Vandaag bespreken we Google Apps Script.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Deze voorbeelden illustreren het basisgebruik, het inbedden van expressies en het creëren van strings over meerdere regels met geïnterpoleerde waarden.

## Diepere Duik
Template literals, inclusief string interpolatie, zijn geïntroduceerd in ECMAScript 2015 (ES6) en vervolgens overgenomen in Google Apps Script. Vóór dit, moesten programmeurs puur vertrouwen op string concatenatie, wat onhandig kon worden voor complexe strings of bij het integreren van veel variabele waarden.

```javascript
// Oude manier (voor ES6)
var user = 'Bob';
console.log('Hallo, ' + user + '!');
```

Hoewel string interpolatie een krachtige functie is, is het belangrijk om rekening te houden met de contexten waarin het wordt gebruikt. Bijvoorbeeld, het direct inbedden van gebruikersinvoer zonder de juiste sanitering kan leiden tot beveiligingsproblemen, zoals injectie-aanvallen. Google Apps Script-ontwikkelaars moeten ervoor zorgen dat alle dynamische content die in strings wordt geïnterpoleerd, op de juiste wijze wordt gecontroleerd of gesaneerd.

In vergelijking met andere programmeertalen bestaat het concept van string interpolatie veelvuldig, met variërende syntax. Python gebruikt f-strings of de `format` methode, Ruby gebruikt `#{}` binnen dubbel aangehaalde strings, en veel moderne talen hebben vergelijkbare functies aangenomen vanwege de leesbaarheid en het gemak dat ze bieden.

Hoewel Google Apps Script geen extra interpolatiefuncties biedt bovenop de door ECMAScript-standaarden geboden functies, is de aanwezige functionaliteit krachtig en voldoende voor de meeste gebruikssituaties. Ontwikkelaars afkomstig uit talen met meer uitgebreide interpolatiemechanismen moeten mogelijk hun verwachtingen bijstellen, maar zullen waarschijnlijk de eenvoud en efficiëntie van template literals in Google Apps Script waarderen.
