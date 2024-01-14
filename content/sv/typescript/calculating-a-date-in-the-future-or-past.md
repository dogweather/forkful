---
title:    "TypeScript: Beräkna ett datum i framtiden eller förflutna"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller i det förflutna kan vara användbart i många olika situationer. Det kan till exempel hjälpa dig att planera dina aktiviteter eller att hålla koll på förfallodatum för viktiga dokument.

## Så här gör du

För att kunna beräkna ett datum i TypeScript behöver du först importera "Date" från standardbiblioteket. Sedan kan du använda metoden "setDate()" för att ange ett specifikt datum och metoden "getDate()" för att få tillbaka det nya datumet som en sträng. Nedan följer ett exempel på hur detta kan se ut i kod:

```TypeScript
import { Date } from "standardbibliotek"

let today = new Date();
let futureDate = new Date();
futureDate.setDate(today.getDate() + 7);
console.log("I dag är det " + today.getDate());
console.log("Om en vecka är det " + futureDate.getDate());
```

Detta kommer att generera följande output:

```
I dag är det 12
Om en vecka är det 19
```

## Djupdykning

Det finns många olika sätt att beräkna datum i TypeScript, beroende på vilken typ av datum du vill ha och vilka parametrar du vill använda. Förutom att använda metoden "setDate()" kan du också använda metoden "setFullYear()" för att sätta ett specifikt år, "setMonth()" för att sätta en specifik månad och "setHours()" för att sätta en specifik tid på dagen. Det finns också en rad olika inbyggda metod för att manipulera datum, till exempel "getDate()", "getMonth()" och "getFullYear()".

## Se även

- [TypeScript: Date](https://www.typescriptlang.org/docs/handbook/dates-and-times.html) 
- [TypeScript: Datatypes and Variables](https://www.typescriptlang.org/docs/handbook/variables.html)
- [TypeScript: Working with Dates](https://www.tutorialspoint.com/typescript/typescript_working_with_dates.htm)