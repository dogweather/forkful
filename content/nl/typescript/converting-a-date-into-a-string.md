---
title:                "Een datum converteren naar een string"
date:                  2024-01-28T21:57:22.489091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum omzetten naar een tekenreeks verandert het datumobject naar een tekstformaat. Programmeurs doen dit voor leesbaarheid, opslag, of om datums aan gebruikers weer te geven.

## Hoe te:

```TypeScript
// Eenvoudige conversie met toLocaleString()
let date = new Date();
let dateString = date.toLocaleString();
console.log(dateString); // "4/3/2023, 13:15:30" (varieert op basis van locatie)

// ISO formaat met toISOString()
let isoString = date.toISOString();
console.log(isoString); // "2023-04-03T13:15:30.000Z"

// Aangepast formaat met toLocaleDateString()
let customString = date.toLocaleDateString('en-US', {
  jaar: 'numeric',
  maand: 'long',
  dag: 'numeric',
});
console.log(customString); // "April 3, 2023"
```

## Diepgaand

Beschouw het tekenreeksformaat van een datum als zijn paspoort, waarmee het kan reizen over systeemgrenzen - van databases naar webpagina's. Historisch gezien hebben we geworsteld met inconsistente datumformaten, wat de reden is waarom standaarden zoals ISO 8601 werden ingevoerd. Dit vereenvoudigt de datumuitwisseling wereldwijd.

Alternatieven voor ingebouwde methoden? Bibliotheken! Moment.js was jarenlang de go-to, maar tegenwoordig zijn date-fns of Luxon de voorkeur - ze zijn lichter en modulairder.

De essentie van deze conversies ligt in de gebruikte methoden. `toLocaleString()` leunt op de lokale instelling van de gebruiker, wat het perfect maakt voor weergave aan gebruikers. `toISOString()` blijft echter trouw aan het ISO 8601-formaat, wat briljant is voor het serialiseren en opslaan van datums in een standaardformaat. En `toLocaleDateString()` geeft je controle over de verschijning, en komt tegemoet aan specifieke stijlbehoeften.

## Zie ook

- [Date Object - MDN Webdocs](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns documentatie](https://date-fns.org/docs/Getting-Started)
- [Luxon documentatie](https://moment.github.io/luxon/)
- [ISO 8601 Datum en tijd formaat](https://www.iso.org/iso-8601-date-and-time-format.html)
