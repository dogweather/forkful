---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:36.660782-07:00
description: "Werken met XML betekent het parseren, manipuleren en schrijven van XML-gegevens\
  \ met behulp van programmeren. Programmeurs gaan om met XML om data uit te\u2026"
lastmod: 2024-02-19 22:05:09.623463
model: gpt-4-0125-preview
summary: "Werken met XML betekent het parseren, manipuleren en schrijven van XML-gegevens\
  \ met behulp van programmeren. Programmeurs gaan om met XML om data uit te\u2026"
title: Werken met XML
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML betekent het parseren, manipuleren en schrijven van XML-gegevens met behulp van programmeren. Programmeurs gaan om met XML om data uit te wisselen tussen verschillende systemen, voor configuratiebestanden of wanneer zij werken met standaarden zoals SOAP die op XML vertrouwen.

## Hoe:
```TypeScript
import { parseString } from 'xml2js';

// Voorbeeld XML
const xml = `<note>
                <to>Gebruiker</to>
                <from>Auteur</from>
                <heading>Herinnering</heading>
                <body>Vergeet de vergadering niet!</body>
             </note>`;

// Parseer XML naar JSON
parseString(xml, (err, resultaat) => {
    if(err) gooi err;
    console.log(resultaat);
});

// Ervan uitgaande dat het parsen succesvol was, zou de uitvoer er zo uit kunnen zien:
// { note:
//    { to: ['Gebruiker'],
//      from: ['Auteur'],
//      heading: ['Herinnering'],
//      body: ['Vergeet de vergadering niet!'] } 
}
```

## Diepgaande Duik
XML, ofwel Extensible Markup Language, is er al sinds de late jaren '90. De zelfbeschrijvende aard en het mens-leesbare formaat hebben het al vroeg populair gemaakt voor verschillende toepassingen zoals RSS-feeds, configuratiebeheer en zelfs kantoor documentformaten zoals Microsoft Office Open XML. Maar, het is langdradig vergeleken met JSON, en de aandacht is verschoven. JSON heeft de spotlight gekregen voor webgebaseerde API's vanwege zijn lichtgewicht en native JavaScript-compatibiliteit.

Desalniettemin is XML niet dood. Het wordt gebruikt in grootschalige ondernemingssystemen en voor documentstandaards die niet naar JSON zijn verschoven. Tools zoals `xml2js` voor TypeScript of `lxml` in Python bewijzen dat er een voortdurende behoefte is aan XML-manipulatie in programmering.

TypeScript heeft geen ingebouwde ondersteuning voor XML zoals het dat voor JSON doet. In plaats daarvan werk je met bibliotheken. `xml2js` is een voorbeeld. Het transformeert XML naar JSON, waardoor de gegevens gemakkelijker voor JavaScript-experts zijn om mee te werken.

## Zie Ook
- [MDN Web Docs over XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm-pakket](https://www.npmjs.com/package/xml2js)
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/)
