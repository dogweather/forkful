---
title:                "TypeScript: Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller i det förflutna kan vara väldigt användbart för en programmerare. Det kan hjälpa till att skapa dynamiska applikationer som bygger på specifika datum, såsom en kalender eller en påminnelsefunktion. Det kan också vara användbart för att beräkna hur lång tid det tar mellan två datum eller för att hantera olika tidszoner.

## Så här gör du

För att kunna beräkna ett datum i TypeScript behöver du använda dig av det inbyggda Date-objektet och dess metoder. Här nedan är ett exempel på hur du kan beräkna ett datum som ligger en vecka framåt från dagens datum:

```TypeScript
let idag = new Date();
let ett_vecka_fram = new Date();

ett_vecka_fram.setDate(idag.getDate() + 7);

console.log(ett_vecka_fram.toDateString());
// Output: "Tue Nov 16 2021"
```

För att beräkna ett datum i det förflutna använder du istället metoden `setDate()` med ett negativt värde. Här är ett exempel på hur du kan beräkna ett datum som ligger en vecka bakåt från dagens datum:

```TypeScript
let idag = new Date();
let ett_vecka_bak = new Date();

ett_vecka_bak.setDate(idag.getDate() - 7);

console.log(ett_vecka_bak.toDateString());
// Output: "Tue Oct 26 2021"
```

För att hantera olika tidszoner kan du använda metoden `setUTCDate()` istället för `setDate()`. Detta kommer att se till att datumet inte påverkas av din aktuella tidszon.

## Djupdykning

Date-objektet i JavaScript och TypeScript följer specifikationerna från ISO 8601, vilket innebär att det följer den gregorianska kalendern. Detta kan vara viktigt att tänka på om du behöver hantera olika kalendrar i din applikation.

Det finns också flera olika metoder inom Date-objektet som kan vara användbara för att manipulera och hämta information från datum, såsom `setMonth()`, `setFullYear()` och `getDay()`. Det kan vara värt att utforska dessa för att få en djupare förståelse för hur man kan hantera datum i JavaScript och TypeScript.

## Se även

- [Date-objektet i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [Moment.js biblioteket för att hantera datum och tider](https://momentjs.com/)
- [ISO 8601 specifikationer för datum och tider](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Klockrena tips för hantering av datum och tider i JavaScript](https://css-tricks.com/everything-you-need-to-know-about-date-in-javascript/)