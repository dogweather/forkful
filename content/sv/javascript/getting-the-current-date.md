---
title:                "Javascript: Få den aktuella datuminformationen"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Det är vanligt att behöva få tag på dagens datum när man programmerar i JavaScript. Det kan till exempel vara för att visa dagens datum på en hemsida eller för att beräkna hur lång tid en viss process har tagit. Genom att lära sig hur man hämtar dagens datum i JavaScript kan du lägga till en användbar funktion till dina projekt.

## Hur man gör

För att få tag på dagens datum i JavaScript kan du använda dig av Date-objektet. Genom att anropa Date-konstruktorn utan några parametrar får du ett objekt som representerar dagens datum och tid. Du kan sedan använda olika metoder för att hämta specifika delar av datumet.

```javascript
let dagensDatum = new Date();
console.log(dagensDatum);
// Output: Sat Apr 24 2021 14:32:16 GMT+0200 (Central European Summer Time)

// Hämta dagens datum
let dag = dagensDatum.getDate();
console.log(dag);
// Output: 24

// Hämta månad
let månad = dagensDatum.getMonth() + 1; // Månader är index-baserade, därför lägger vi till 1
console.log(månad);
// Output: 4

// Hämta år
let år = dagensDatum.getFullYear();
console.log(år);
// Output: 2021
```

## Djupdykning

Det finns många olika metoder och egenskaper som du kan använda för att hämta information om dagens datum i JavaScript. Förutom de som nämndes ovan finns det bland annat också metoder för att hämta dagens veckodag, timme och minut. Du kan också läsa på om hur man formaterar datumet på olika sätt beroende på dina behov.

Det är också viktigt att komma ihåg att datum skiljer sig åt beroende på vilken tidszon du befinner dig i. Därför kan det vara bra att använda sig av bibliotek som Moment.js för att få mer precisa resultat.

## Se även

- [Date](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Date Formatting in Javascript - Ultimate Guide](https://programmingwithmosh.com/javascript/date-formatting-javascript/)