---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:37:34.216456-07:00
simple_title:         "Tolka ett datum från en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "parse:a" ett datum innebär att man översätter en datumsträng till ett format som JavaScript kan förstå och hantera. Programmerare gör detta för att kunna manipulera och jämföra datum, t.ex. när man arbetar med användarinmatningar eller API-respons.

## Hur man gör:
```javascript
// Exempel 1: Använda Date-konstruktorn
const dateString = '2023-04-01T12:00:00Z';
const date = new Date(dateString);
console.log(date); // Visar datumobjekt baserat på strängen

// Exempel 2: Använda Date.parse
const timestamp = Date.parse('2023-04-01T12:00:00Z');
console.log(new Date(timestamp)); // Konverterar timestamp till datumobjekt

// Exempel 3: Parse med moment.js (ett populärt bibliotek)
// Obs! Först behöver du inkludera moment.js i ditt projekt
const momentDate = moment('2023-04-01T12:00:00Z');
console.log(momentDate.toDate()); // Konverterar Moment-objekt till datumobjekt
```

## Djupdykning
I JavaScripts barndom (från 1995 och framåt) var datumhantering bökig. `Date.parse` och `Date`-konstruktorn har länge varit JavaScripts inbyggda metoder för att tolka datumsträngar. Dessa metoder stödjer officiellt ISO 8601-formatet, men deras beteende kan variera beroende på browser. Det har lett till populariteten för bibliotek som Moment.js, Date-fns eller Luxon, som erbjuder mer konsekvent parse:ning och en uppsjö av bekväma funktioner.

`Date`-konstruktorn kan tolka de flesta standardformat, men problem uppstår med tidszoner och browser-specifik beteende. `Date.parse` returnerar ett timestamp som är antalet millisekunder sedan epoch (1 januari 1970) och ger dig inte direkt ett datumobjekt. Biblioteken hanterar detta snyggt och erbjuder parse:ning med tidszonsstöd.

Beroende på vilket bibliotek du använder kan detaljerna och implementationen variera, men grunden är densamma: parse:a strängen till något användbart.

## Se även
- MDN Web Docs för `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js dokumentation: https://momentjs.com/docs/
- Date-fns biblioteket: https://date-fns.org/
- Luxon biblioteket: https://moment.github.io/luxon/#/ 

Varje länk är en guldgruva för vidare läsning och fördjupad förståelse kring datumhantering i JavaScript.
