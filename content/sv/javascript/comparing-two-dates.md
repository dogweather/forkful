---
title:                "Javascript: Jämförelse av två datum"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle man vilja jämföra två datum i Javascript? Det finns flera situationer där denna funktion kan vara användbar, till exempel när man vill sortera eller filtrera data baserat på datum, eller när man vill visa hur många dagar som har gått mellan två händelser.

## Så här gör du
För att jämföra två datum i Javascript kan du använda Date-objektet och dess inbyggda metoder. Först måste du skapa två Date-objekt för de datum du vill jämföra. Dessa kan antingen skapas genom att ange ett specifikt datum eller genom att hämta datum från ett annat objekt eller en variabel. Här är ett exempel på hur man skulle kunna göra det:

```Javascript
var date1 = new Date('December 31, 2020');
var date2 = new Date(); // Detta skapar ett Date-objekt med dagens datum
```

Nästa steg är att använda Date-objektets inbyggda metod `getTime()` för att få ut tiden (i millisekunder) från varje datum. Dessa tider kan sedan jämföras med hjälp av vanliga jämförelseoperatorer. Om man till exempel vill kolla om `date1` är tidigare än `date2`, kan man använda följande kod:

```Javascript
if (date1.getTime() < date2.getTime()) {
  console.log('date1 är tidigare än date2');
}
```

Här är några fler exempel på hur du kan jämföra datum i Javascript:

```Javascript
// Kolla om två datum är lika
if (date1.getTime() === date2.getTime()) {
  console.log('Det är samma dag');
}

// Kolla om ett datum är senare än ett annat
if (date1.getTime() > date2.getTime()) {
  console.log('date1 är senare än date2');
}
```

## Djupdykning
När man jämför datum i Javascript finns det några saker att tänka på. Till exempel kan man också använda andra metoder som `getFullYear()` och `getMonth()` för att få ut specifika delar av ett datum och jämföra dem. Det är också viktigt att tänka på tidszoner när man arbetar med datum i Javascript, eftersom olika länder och regioner kan ha olika tidszoner. Man bör också vara medveten om att datum i Javascript är en tidpunkt, inte bara en kalenderdag, så om man jämför datum med datum och tid måste man se till att ta hänsyn till tiden också.

## Se även
- [MDN webbdokumentation för Date-objektet](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Jämföra datum i Javascript med moment.js](https://momentjs.com/docs/#/query/is-same/)