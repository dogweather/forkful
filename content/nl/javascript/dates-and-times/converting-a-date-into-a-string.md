---
aliases:
- /nl/javascript/converting-a-date-into-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:17.795327-07:00
description: "Datum naar string conversie transformeert een Datum-object naar een\
  \ leesbaar tekstformaat, omdat mensen \"1 april 2023\" verkiezen boven cryptische\u2026"
lastmod: 2024-02-18 23:09:02.288943
model: gpt-4-0125-preview
summary: "Datum naar string conversie transformeert een Datum-object naar een leesbaar\
  \ tekstformaat, omdat mensen \"1 april 2023\" verkiezen boven cryptische\u2026"
title: Een datum converteren naar een string
---

{{< edit_this_page >}}

## Wat & Waarom?
Datum naar string conversie transformeert een Datum-object naar een leesbaar tekstformaat, omdat mensen "1 april 2023" verkiezen boven cryptische tijdscodes. Programmeurs doen dit voor duidelijkheid in gebruikersinterfaces en om datums te formatteren voor opslag of netwerkoverdracht.

## Hoe te:
JavaScript heeft ingebouwde methoden om datums naar strings te converteren. Hier is hoe je ze gebruikt:

```javascript
const now = new Date();

// toLocaleString() - lokaal formaat
console.log(now.toLocaleString()); // '1-4-2023 12:00:00'

// toString() - standaard formaat
console.log(now.toString()); // 'Za Apr 01 2023 12:00:00 GMT+0100 (Midden-Europese standaardtijd)'

// toISOString() - ISO formaat (geweldig voor databases/netwerk)
console.log(now.toISOString()); // '2023-04-01T11:00:00.000Z'
```

## Diepgaande duik
Vroeger was datum naar string conversie een rommeltje—geen standaarden, alleen een hoop aangepaste functies. Gelukkig greep ECMAScript in, door het Datum-object te standaardiseren in ES5 en de superhandige `toISOString()` in ES5.1 toe te voegen.

Alternatieven voor de native methoden zijn bibliotheken zoals `moment.js` en `date-fns`, die meer controle en tijdzonebehandeling bieden, maar ze vergroten de omvang van je project.

Onder de motorkap, wanneer je een datum-naar-string methode aanroept, interageert JavaScript met de lokale instellingen van het systeem en tijdzone-informatie om de stringuitvoer te genereren. In tegenstelling, `toISOString()` geeft altijd een UTC-tijd terug (de 'Z' staat voor 'Zulu-tijd' of nul offset van UTC).

## Zie ook
- [MDN Web Docs – Date](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 Datum- en tijdformaat](https://www.iso.org/iso-8601-date-and-time-format.html)
- [date-fns](https://date-fns.org/)
