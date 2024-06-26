---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:48:45.880465-07:00
description: "Hoe te: Google Apps Script, gebaseerd op JavaScript, biedt verschillende\
  \ methoden om een string te kapitaliseren, zij het zonder een ingebouwde functie.\u2026"
lastmod: '2024-03-13T22:44:50.313306-06:00'
model: gpt-4-0125-preview
summary: Google Apps Script, gebaseerd op JavaScript, biedt verschillende methoden
  om een string te kapitaliseren, zij het zonder een ingebouwde functie.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe te:
Google Apps Script, gebaseerd op JavaScript, biedt verschillende methoden om een string te kapitaliseren, zij het zonder een ingebouwde functie. Hier zijn een paar bondige voorbeelden:

**Methode 1: Gebruik makend van charAt() en slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Voorbeeldgebruik
let result = capitalizeString('hallo, wereld');
console.log(result);  // Uitvoer: Hallo, wereld
```

**Methode 2: Gebruik makend van een Regex**

Voor degenen die de voorkeur geven aan een op regex gebaseerde oplossing om randgevallen eleganter aan te pakken:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Voorbeeldgebruik
let result = capitalizeStringRegex('hallo, wereld');
console.log(result);  // Uitvoer: Hallo, wereld
```

Beide methoden zorgen ervoor dat het eerste teken van de reeks een hoofdletter is en de rest kleine letters zijn, geschikt voor een verscheidenheid aan toepassingen, waaronder maar niet beperkt tot het manipuleren van Google Sheets of documentbewerking via Apps Script.

## Diepgaande Duik
Het kapitaliseren van strings in Google Apps Script is eenvoudig, met gebruikmaking van de krachtige stringmanipulatiemogelijkheden van JavaScript. Historisch gezien bieden talen zoals Python ingebouwde methoden zoals `.capitalize()` om dit te bereiken, wat een kleine extra stap betekent voor JavaScript- en Apps Script-programmeurs. Het ontbreken van een ingebouwde functie in JavaScript/Google Apps Script bevordert echter flexibiliteit en een dieper begrip van stringmanipulatietechnieken.

Voor complexe scenario's, zoals het kapitaliseren van elk woord in een string (Titelvorm), kunnen programmeurs regex-methoden combineren met `split()` en `map()` functies om elk woord afzonderlijk te verwerken. Hoewel Google Apps Script geen directe methode biedt voor het kapitaliseren van strings, biedt het gebruik van bestaande JavaScript-stringmanipulatiemethoden voldoende flexibiliteit, waardoor ontwikkelaars strings efficiënt kunnen behandelen volgens hun specifieke behoeften.

In gevallen waar prestaties en efficiëntie van het grootste belang zijn, is het vermeldenswaard dat directe stringmanipulatie mogelijk performanter kan zijn dan regex, vooral bij langere strings of bewerkingen binnen grote lussen. Echter, voor de meeste praktische toepassingen binnen Google Apps Script bieden beide benaderingen betrouwbare oplossingen.
