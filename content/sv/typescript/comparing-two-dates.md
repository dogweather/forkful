---
title:                "TypeScript: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig färdighet för alla TypeScript-programmerare. Det finns många situationer där vi behöver avgöra vilket datum som är senare eller äldre, eller om två datum är samma. Det är också viktigt att veta hur man hanterar olika format av datum för att undvika felaktiga resultat och buggar.

## Hur man gör

För att jämföra två datum i TypeScript behöver vi använda inbyggda metoder som Date och Date.prototype. Vi kan också använda bibliotek som moment.js för mer komplexa beräkningar.

Först måste vi skapa två Date-objekt som vi vill jämföra. Vi kan göra det genom att ange året, månaden och dagen som parametrar till Date-konstruktorn. Till exempel: 

```typescript
let startDate = new Date(2021, 5, 1);
let endDate = new Date(2022, 0, 1);
```

För att jämföra om startDate är senare än endDate använder vi metoden `.getTime()` som returnerar antalet millisekunder sedan 1 januari 1970. Sedan kan vi helt enkelt använda ett vanligt if-sats för att avgöra om startDate är senare än endDate:

```typescript
if (startDate.getTime() > endDate.getTime()) {
  console.log("startDate är senare än endDate");
} else {
  console.log("endDate är senare än startDate");
}
```

Om vi istället bara vill jämföra dagar, månader eller år kan vi använda Date-metoderna `.getDate()`, `.getMonth()` och `.getFullYear()`. Till exempel:

```typescript
if (startDate.getFullYear() === endDate.getFullYear()) {
  console.log("Båda datum har samma år");
}
```

## Djupdykning

När vi jämför två datum i TypeScript är det viktigt att tänka på tidszonen. Om vi använder `.getTime()`-metoden kan ett datum i en annan tidszon ge oss ett annat resultat. Det är också viktigt att hantera potentiella fel som kan uppstå om vi inte anger alla parametrar korrekt i Date-konstruktorn, som att råka byta plats på månad och dag.

Det finns också andra metoder som kan vara användbara när man hanterar datum i TypeScript, som `.setDate()`, `.setMonth()` och `.setFullYear()` som ändrar ett datumobjekt utan att behöva skapa ett nytt.

## Se även

- [Date - MDN](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js](https://momentjs.com/)
- [Stack Overflow: Comparing dates in TypeScript](https://stackoverflow.com/questions/35169127/comparing-dates-in-typescript)