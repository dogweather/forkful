---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:05.359559-07:00
description: "Een datum uit een string parsen betekent het omzetten van tekst die\
  \ een datum vertegenwoordigt naar een datumobject. Programmeurs doen dit omdat het\u2026"
lastmod: 2024-02-19 22:05:10.298051
model: gpt-4-0125-preview
summary: "Een datum uit een string parsen betekent het omzetten van tekst die een\
  \ datum vertegenwoordigt naar een datumobject. Programmeurs doen dit omdat het\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum uit een string parsen betekent het omzetten van tekst die een datum vertegenwoordigt naar een datumobject. Programmeurs doen dit omdat het cruciaal is voor het omgaan met datums in applicaties, zoals het sorteren van evenementen of het filteren van logs.

## Hoe te:

In JavaScript kun je een datum uit een string parsen met de `Date` constructor of bibliotheken zoals `Date-fns` en `Moment.js`. Hier is de native manier:

```Javascript
let dateString = "2023-04-01T12:00:00Z";
let parsedDate = new Date(dateString);

console.log(parsedDate); // Geeft uit: Sat Apr 01 2023 12:00:00 GMT+0000 (Gecoördineerde Universele Tijd)
```

Voor meer controle en consistentie kunnen bibliotheken nuttig zijn:

```Javascript
// Parsen met Moment.js
const moment = require('moment');
let momentDate = moment("2023-04-01");
console.log(momentDate.toString()); // Geeft uit: Sat Apr 01 2023 00:00:00 GMT+0000

// Parsen met Date-fns
const dateFns = require('date-fns/parse');
let dateFnsDate = dateFns("2023-04-01", "yyyy-MM-dd", new Date());
console.log(dateFnsDate); // Geeft uit: Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

## Diepere Duik

JavaScript heeft ingebouwde datumafhandeling, maar die was niet altijd geweldig. Eerdere versies hadden problemen met consistentie, tijdzones en opmaak. Mensen raakten vaak gefrustreerd en ontwikkelden hun eigen oplossingen of gebruikten bibliotheken van derden zoals `Moment.js`, die meer functies en betere parseeropties boden.

In de loop der tijd verbeterde JavaScript, en nieuwe bibliotheken zoals `Date-fns` en `Luxon` kwamen tevoorschijn, gericht op kleinere, snellere en meer modulaire hulpprogramma’s. Een alternatief is de constructor `Intl.DateTimeFormat`, onderdeel van de Internationaliserings-API, die taalgevoelige datum- en tijdopmaak mogelijk maakt.

Hier is het fijne detail: het parsen is risicovol vanwege verschillen in formaten. De `Date` constructor in JavaScript kan onvoorspelbaar handelen met dubbelzinnige datumstrings. Het is het beste om een gestandaardiseerd formaat zoals ISO 8601 (`YYYY-MM-DDTHH:mm:ss.sssZ`) te gebruiken om verwarring te voorkomen. Bibliotheken komen met hun eigen parseerregels en toegevoegde functies om de eigenaardigheden van datum-tijdformaten aan te pakken, zodat ontwikkelaars veelvoorkomende valkuilen kunnen vermijden.

Onthoud altijd voorzichtig te zijn met tijdzones bij het parsen van datums; ze kunnen de correctheid van je datumlogica maken of breken.

## Zie Ook

- MDN Web Docs over `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/docs/#/parsing/string/
- Date-fns Documentatie: https://date-fns.org/v2.28.0/docs/parse
- Internationalisatie-API: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
