---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:18.625988-07:00
description: "Een datum uit een string parsen houdt in dat tekst die een datum vertegenwoordigt\
  \ wordt omgezet in een datumobject, waardoor programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.341881-06:00'
model: gpt-4-0125-preview
summary: "Een datum uit een string parsen houdt in dat tekst die een datum vertegenwoordigt\
  \ wordt omgezet in een datumobject, waardoor programmeurs\u2026"
title: Een datum ontleden uit een string
weight: 30
---

## Wat & Waarom?

Een datum uit een string parsen houdt in dat tekst die een datum vertegenwoordigt wordt omgezet in een datumobject, waardoor programmeurs datumgerelateerde bewerkingen kunnen uitvoeren zoals vergelijkingen, rekenkundige bewerkingen en formatteren. Het is essentieel voor het verwerken van gebruikersinvoer, het verwerken van gegevens uit externe bronnen en het beheren van datums in verschillende formaten, vooral in toepassingen die te maken hebben met planning, gegevensanalyse of enige vorm van tijdgebaseerde registraties.

## Hoe te:

In Google Apps Script, dat is gebaseerd op JavaScript, heb je verschillende benaderingen om een datum uit een string te parsen. Hieronder volgen voorbeelden met zowel de native JavaScript-methoden als de hulpprogramma's van Google Apps Script.

**Gebruik van `new Date()` constructor:**

De eenvoudigste manier om een string in een datum in Google Apps Script te parsen, is het gebruik van de constructor van het `Date` object. Dit vereist echter dat de datumstring in een door de Date.parse() methode herkend formaat staat (bijv. YYYY-MM-DD).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Logboeken Zat 01 Apr 2023 00:00:00 GMT+0000 (UTC)
```

**Gebruik van `Utilities.parseDate()`:**

Voor meer flexibiliteit, met name bij aangepaste datumformaten, biedt Google Apps Script `Utilities.parseDate()`. Deze methode stelt je in staat het datumformaat, de tijdzone en de locale op te geven.

```javascript
const dateString = '01-04-2023'; // DD-MM-JJJJ
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // Logboeken Zat 01 Apr 2023 00:00:00 GMT+0000 (UTC) afhankelijk van de tijdzone van het script
```

Let op: Hoewel `Utilities.parseDate()` meer controle biedt, kan het gedrag ervan variëren op basis van de tijdzone van het script, dus het is cruciaal om de tijdzone expliciet op te geven als je applicatie datums over meerdere regio's heen behandelt.

## Uitdieping

Het parsen van datums in programmeertalen is historisch gezien beladen geweest met uitdagingen, voornamelijk vanwege de verscheidenheid aan datumformaten en de complexiteit van tijdzones. De benadering van Google Apps Script, voornamelijk afgeleid van JavaScript, streeft ernaar dit te vereenvoudigen door zowel het eenvoudige `Date` object als de meer veelzijdige functie `Utilities.parseDate()` aan te bieden. Elke methode heeft echter zijn beperkingen; bijvoorbeeld, vertrouwen op de `Date` constructor met strings leidt tot inconsistenties in verschillende omgevingen vanwege verschillende interpretaties van datumformaten. Aan de andere kant vereist `Utilities.parseDate()` een duidelijker begrip van het formaat, de tijdzone en de locale, waardoor het iets complexer maar betrouwbaarder is voor specifieke behoeften.

Alternatieve bibliotheken of diensten, zoals Moment.js (nu Luxon aanbevelend voor nieuwe projecten), bieden rijkere functionaliteiten en betere zonebehandeling, waarmee veel van deze uitdagingen worden aangepakt. Toch, in de context van Google Apps Script, waar externe bibliotheken beperkingen hebben, wordt het cruciaal om de ingebouwde methoden effectief te begrijpen en te benutten. Programmeurs die uit andere talen komen, kunnen de nuances van datumbehandeling in Google Apps Script uniek uitdagend vinden, maar kunnen met een diep begrip van de beschikbare hulpmiddelen en zorgvuldige overweging van de wereldwijde aard van hun applicaties, robuuste datumverwerking bereiken.
