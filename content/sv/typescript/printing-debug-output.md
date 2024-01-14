---
title:                "TypeScript: Utskrift av felsökningsspårning"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Att utskriva debug-utdata är ett viktigt verktyg för att felsöka och förstå vad som händer i din TypeScript-kod. Det kan hjälpa dig att identifiera och åtgärda fel eller felaktiga beteenden, vilket kan spara dig mycket tid och frustration i slutändan.

## Hur man gör det
Det finns flera olika sätt att skriva ut debug-utdata i din TypeScript-kod. Här är några exempel att prova på:

```TypeScript
// Skriva ut en sträng
const namn: string = "Anna";
console.log(`Hej ${namn}!`); // Output: Hej Anna!

// Skriva ut en array
const nummer: number[] = [1, 2, 3];
console.log(`Det här är en array av nummer: ${nummer}`); // Output: Det här är en array av nummer: [1, 2, 3]

// Skriva ut ett objekt
const person = {
  namn: "Johan",
  ålder: 25,
  yrke: "Utvecklare"
};
console.log(`Här är lite information om ${person.namn}:`, person); // Output: Här är lite information om Johan: { namn: "Johan", ålder: 25, yrke: "Utvecklare" }
```

Detta är bara några enkla exempel, men möjligheterna är oändliga. Du kan använda `console.log()` för att skriva ut nästan vad som helst - variabler, funktioner, lokala och globala värden, och så vidare.

## Djupdykning
Nu när du vet hur man skriver ut debug-utdata, låt oss ta en djupare titt på varför det är så användbart. Ofta när du skriver kod möter du problem eller frågor som du behöver lösa. Genom att skriva ut debug-utdata i din kod, kan du se exakt vad som händer vid en viss punkt. Detta gör det lättare att förstå och identifiera problemet, vilket i sin tur hjälper dig att lösa det snabbare.

Att skriva ut debug-utdata är också en bra vana att utveckla som utvecklare. Genom att se vad din kod gör i varje steg, kan du lära dig mer om språket och bättra på din egen kodningsteknik.

## Se även
Här är några länkar för att hjälpa dig att lära dig mer om debugging i TypeScript:

- [TypeScript felsökning och loggning](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [De 5 vanligaste felsökningsteknikerna i TypeScript](https://blog.logrocket.com/what-are-the-5-most-common-typescript-debugging-techniques/)
- [Felsökningsverktyg för TypeScript](https://blog.angularindepth.com/typescript-debugging-for-newbies-9f83e4285fa8)