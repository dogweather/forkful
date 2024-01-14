---
title:                "Javascript: Utskrift av felsökningsutdata"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsspråk finns det funktioner för att skriva ut debug-utdata till konsolen eller terminalen. Men varför skulle man ens vilja göra det? Svaret är enkelt: det är ett användbart verktyg för att felsöka och förstå koden man skriver. Genom att skriva ut olika värden under körningen av programmet kan man få en bättre förståelse för vad som händer i koden, hitta eventuella fel och se hur olika variabler förändras i realtid.

## Hur man gör det

I Javascript finns det flera sätt att skriva ut debug-utdata. Det enklaste är att använda "console.log()" funktionen. Detta kommer att skriva ut värdet av en variabel eller ett meddelande till konsolen. Till exempel:

```Javascript
let num1 = 5;
let num2 = 10;
console.log(num1 + num2); // Output: 15
```

Man kan också använda "console.error()" för att markera viktiga fel som behöver uppmärksamhet eller "console.warn()" för att varna om något som kan orsaka problem. Om man vill visa ett objekt eller en array i konsolen kan man använda "console.dir()".

En annan användbar funktion är "console.table()", som visar data i form av en tabell. Detta kan vara särskilt användbart för att visa objekt eller data med flera kolumner.

## Deep Dive

Förutom de grundläggande sätten att skriva ut debug-utdata finns det också andra sätt att anpassa utskriften. Man kan till exempel använda placeholders för att visa specifika värden vid olika tillfällen. Detta görs genom att använda "%" tecken i koden och sedan ange värdet efter en komma.

```Javascript
let name = "Anna";
let age = 25;
console.log("%s är %d år gammal.", name, age); // Output: Anna är 25 år gammal.
```

Det går också att använda "console.group()" och "console.groupEnd()" för att gruppera flera utskrifter tillsammans i konsolen. Detta kan vara användbart om man skriver ut flera värden för samma databasförfrågan eller funktion.

Slutligen kan man använda "console.time()" och "console.timeEnd()" för att mäta prestanda av kodstycken. Genom att omringa ett kodavsnitt med dessa funktioner kan man se hur lång tid det tar att köra och identifiera eventuella flaskhalsar i koden.

## Se även

Här är några länkar som kan vara användbara för att lära sig mer om att skriva ut debug-utdata i Javascript:

- [MDN dokumentation om console](https://developer.mozilla.org/sv-SE/docs/Web/API/console)
- [10 sätt att använda console i en bättre programmeringsmiljö](https://dev.to/zackshapiro/10-ways-to-use-console-in-a-better-programming-environment-31ph)
- [Debugging i Javascript for Nybörjare](https://www.educative.io/blog/javascript-debugging-for-beginners)