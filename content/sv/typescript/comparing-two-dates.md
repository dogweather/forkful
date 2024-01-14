---
title:                "TypeScript: Jämföring av två datum"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en vanlig uppgift inom programmering. Det kan vara till exempel för att bestämma vilket datum som är senare, beräkna antal dagar mellan två datum eller för att filtrera ut specifika datum i en lista. Det är också en användbar funktion för hantering av tidszoners skillnader och summering av arbetsdagar.

## Hur man gör det
För att jämföra datum i TypeScript finns det några användbara metoder som vi kan använda oss av. Vi kan använda det inbyggda Date objektet och dess metoder, eller så kan vi använda ett externt bibliotek som moment.js. Här är några exempel på hur man kan jämföra två datum:

```TypeScript
// Jämför två datum med inbyggda metoder
let datum1 = new Date('2021-01-01');
let datum2 = new Date('2021-02-01');

if (datum1 > datum2) {
  console.log('Datum1 är senare än datum2');
} else if (datum1 < datum2) {
  console.log('Datum2 är senare än datum1');
} else {
  console.log('Datum1 och datum2 är samma datum');
}

// Beräkna antal dagar mellan två datum med moment.js
let antalDagar = moment(datum2).diff(datum1, 'days');

console.log('Antal dagar mellan datum1 och datum2 är ' + antalDagar);
```

Output:
```
Datum2 är senare än datum1
Antal dagar mellan datum1 och datum2 är 31
```

## Djupdykning
När vi jämför datum är det viktigt att förstå hur Datum objektet fungerar. Detta objekt sparar datum och tid i millisekunder från 1 januari 1970 00:00:00 UTC. Det betyder att ju senare datumet är, desto högre millisekunder har den. För att jämföra två datum använder JavaScript's inbyggda metoder, såsom `getTime()` eller `valueOf()`, för att konvertera datum till millisekunder och sedan jämföra dem.

Det är också viktigt att vara medveten om att tidzon och sommartid påverkar hur datum visas och beräknas. Detta kan orsaka problem när man jämför datum på olika platser i världen. Därför är det bäst att använda ett extern bibliotek som moment.js för att hantera dessa skillnader.

## Se även
- [Moment.js dokumentation](https://momentjs.com/docs/)
- [Official TypeScript handbook - Dates](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Jämföra datum i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#comparisons)