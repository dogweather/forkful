---
title:                "Beräkning av ett datum i framtiden eller i det förflutna"
html_title:           "TypeScript: Beräkning av ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller i det förflutna"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett datum i framtiden eller det förflutna är en process som används för att hitta ett datum som är antingen framåt eller bakåt i tiden från ett givet startdatum. Programmerare använder detta för att hantera datum inom program, till exempel att schemalägga uppgifter, visa tidslinjer eller hantera åldersberäkningar.

## Hur gör man:
```TypeScript
// Beräkna datumen för en vecka framåt
let startDatum = new Date();
let veckaSenare = startDatum.getDate() + 7; 
let nyttDatum = new Date(startDatum.setDate(veckaSenare)); 

console.log(nyttDatum); // sample output: 2021-07-14T15:35:37.590Z
```

```TypeScript
// Beräkna tiden innan ett visst datum
let startDatum = new Date('2021-07-01');
let tidsintervall = 30; // antal dagar
let datumInnan = startDatum.getDate() - tidsintervall; 
let nyttDatum = newDate(startDatum.setDate(datumInnan)); 

console.log(nyttDatum); // sample output: 2021-06-01T07:00:00.000Z
```

## Djupdykning:
Beräkning av datum har varit en viktig del av programmeringen sedan början. Det är en vanlig uppgift som ofta används inom olika applikationer. Det finns olika sätt att beräkna datum, till exempel genom att använda inbyggda funktioner i ett programmeringsspråk eller använda speciella bibliotek. Det är också viktigt att tänka på olika tidszoner och hur de kan påverka beräkning av datum.

## Se även:
- [JavaScript Date object](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)