---
title:                "Jämföring av två datum"
html_title:           "TypeScript: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämförelse av två datum är en vanlig uppgift för programmerare. Det är ett sätt att bestämma vilket av två datum som är större eller mindre, och det kan vara användbart vid sortering eller filtering av data.

## Hur gör man:
Du kan använda Date-objektet i TypeScript för att jämföra två datum. Här är ett exempel på hur man kan göra det:

```TypeScript
let date1: Date = new Date(2020, 0, 1);
let date2: Date = new Date(2020, 1, 1);

//För att jämföra om date1 är före date2
if (date1 < date2) {
   console.log("date1 är mindre än date2");
}

//För att jämföra om date1 är efter date2
if (date1 > date2) {
   console.log("date1 är större än date2");
}

//För att jämföra om date1 och date2 är samma datum
if (date1 === date2) {
   console.log("date1 och date2 är samma datum");
}
```

Resultatet av detta skulle vara:

```
date1 är mindre än date2
```

## Djupdykning:
Jämförelse av datum har funnits länge inom programmering och har alltid varit en viktig uppgift för att hantera och analysera datumdata. Innan Date-objektet introducerades användes numeriska värden för datum och tider, vilket var mer komplicerat att hantera.

Det finns också andra sätt att jämföra datum i TypeScript, såsom användning av ```getTime()``` funktionen som returnerar antalet millisekunder som har passerat sedan 1 januari 1970.

När man jämför datum är det också viktigt att ta hänsyn till olika tidszoner och sommartid, vilket kan påverka resultatet av jämförelsen.

## Se även:
- [Date-objektet i TypeScript](https://www.typescriptlang.org/docs/handbook/standard-dates.html)
- [getTime() funktionen i JavaScript](https://www.w3schools.com/jsref/jsref_gettime.asp)
- [Tidszon och sommartid i JavaScript](https://momentjs.com/docs/#/manipulating/timezone/)