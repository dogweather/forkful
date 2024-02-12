---
title:                "Een datum in de toekomst of het verleden berekenen"
aliases:
- nl/javascript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:55:51.363685-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het berekenen van een toekomstige of verleden datum betekent het vinden van een datum een aantal dagen, weken, maanden of jaren vanaf een bepaald punt. Programmeurs hebben dit vaak nodig voor taken zoals het instellen van vervaldatums, herinneringen of het plannen van evenementen.

## Hoe:
Het `Date`-object van JavaScript is je beste vriend voor datagymnastiek. Laten we spelen met enkele voorbeelden:

```javascript
// De datum van vandaag
let today = new Date();
console.log(today); // Geeft de huidige datum en tijd weer

// Bereken een datum 7 dagen in de toekomst
let nextWeek = new Date();
nextWeek.setDate(today.getDate() + 7);
console.log(nextWeek); // Geeft de datum weer voor dezelfde tijd, 7 dagen later

// Bereken een datum 30 dagen in het verleden
let lastMonth = new Date();
lastMonth.setDate(today.getDate() - 30);
console.log(lastMonth); // Geeft de datum weer voor dezelfde tijd, 30 dagen geleden

// Stel een datum in 1 jaar in de toekomst in
let nextYear = new Date();
nextYear.setFullYear(today.getFullYear() + 1);
console.log(nextYear); // Geeft de datum weer voor dezelfde tijd volgend jaar
```
Outputs zijn afhankelijk van wanneer je deze code uitvoert, aangezien `today` je huidige datum-tijd is.

## Diepgaande Duik
Voordat JavaScript ingebouwde datummanipulatiefuncties had, moesten programmeurs handmatig datums berekenen waarbij rekening gehouden moest worden met variaties in maandlengtes, schrikkeljaren en tijdzones - een echte pijn! Met `Date` verdwijnt veel van deze last.

Alternatieven voor het native `Date`-object zijn bibliotheken zoals `moment.js` en `date-fns`, die rijkere syntaxis bieden en eigenaardigheden zoals bugs in zomertijdaanpassingen oplossen.

Wanneer je datums berekent, onthoud: `Date` telt maanden van 0 (januari) tot 11 (december), niet 1-12. En vergeet de schrikkeljaren niet bij het werken met februaridatums.

## Zie Ook
- MDN Web Docs over Datum: https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/
