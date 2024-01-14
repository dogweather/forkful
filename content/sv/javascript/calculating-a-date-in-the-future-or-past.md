---
title:    "Javascript: Beräkning av ett datum i framtiden eller i det förflutna"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Det finns många tillfällen när man behöver beräkna ett datum i framtiden eller förflutet. Kanske när man ska skapa en kalenderapplikation eller planera en resa. I denna bloggpost kommer vi att utforska hur man kan använda Javascript för att utföra dessa beräkningar.

## Hur man gör
För att beräkna ett datum i framtiden eller förflutet i Javascript, behöver vi först skapa ett nytt Date-objekt med hjälp av `new Date()` konstruktorn. Därefter kan vi använda olika metoder för att manipulera detta datum.

För att beräkna ett datum i framtiden lägger vi till antalet millisekunder i framtiden som vi vill att datumet ska vara. Till exempel, om vi vill ha datumet för 5 dagar framöver, lägger vi till 5 dagar motsvarande 5 * 24 * 60 * 60 * 1000 millisekunder till datumet.

```Javascript
const today = new Date();
const futureDate = new Date(today.getTime() + (5 * 24 * 60 * 60 * 1000)); // 5 dagar i millisekunder
console.log(futureDate); // Output: 2021-06-10T10:35:28.348Z
```

För att beräkna ett datum i förflutet drar vi istället antalet millisekunder från datumet. Till exempel, om vi vill ha datumet för 2 veckor sedan, drar vi 2 veckors motsvarande 2 * 7 * 24 * 60 * 60 * 1000 millisekunder från dagens datum.

```Javascript
const today = new Date();
const pastDate = new Date(today.getTime() - (2 * 7 * 24 * 60 * 60 * 1000)); // 2 veckor i millisekunder
console.log(pastDate); // Output: 2021-05-23T10:35:28.348Z
```

## Djupdykning
Det finns även andra metoder som kan användas för att beräkna datum i framtiden och förflutet i Javascript. Till exempel, `setDate()` och `getDate()` för att manipulera och hämta specifika datumvärden som dagar, `setMonth()` och `getMonth()` för månader, och så vidare.

Det finns också bibliotek och paket, som moment.js, som kan förenkla detta beräkningsprocessen och ge mer flexibilitet och funktionalitet.

## Se även
- [Date object in Javascript (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js](https://momentjs.com/)