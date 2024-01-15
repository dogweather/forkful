---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "TypeScript: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förflutna kan vara mycket användbart inom programmering. Det kan hjälpa till med att schemalägga uppgifter eller visa information baserat på ett visst datum.

## Hur man gör det

För att beräkna ett datum i framtiden eller förflutna i TypeScript behöver vi använda oss av Date-objektet och dess metoder. Nedan finns ett exempel på hur man kan beräkna ett datum fem dagar framåt:

```TypeScript
let nuvarandeDatum = new Date(); // Skapar ett Date-objekt med dagens datum
// Använder .setDate() för att lägga till 5 dagar på det nuvarande datumet
nuvarandeDatum.setDate(nuvarandeDatum.getDate() + 5); 
console.log(nuvarandeDatum); // Skriver ut det nya datumet i konsolen
```

Detta kommer att skriva ut det nya datumet i konsolen, vilket i detta fall skulle vara fem dagar framåt från idag. Det är även möjligt att beräkna ett datum i förflutna genom att använda .setDate() metoden med en negativ siffra.

### Deep Dive

För att få en djupare förståelse för att beräkna datum i TypeScript är det viktigt att också känna till de andra metoder som Date-objektet har att erbjuda. Nedan är ett par exempel på dessa metoder:

- .setFullYear(): används för att ändra året för ett specifikt datum
- .getMonth(): returnerar månaden för ett specifikt datum (januari är 0 och december är 11)
- .getDay(): returnerar veckodagen för ett specifikt datum (söndag är 0 och lördag är 6)

Genom att kombinera dessa metoder med .setDate() kan vi skapa mer avancerade beräkningar av datum i framtiden eller förflutna.

## Se även

- [Date-objektet i TypeScript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Date-objektet i TypeScript på Typescriptlang.org](https://www.typescriptlang.org/docs/handbook/working-with-dates.html)