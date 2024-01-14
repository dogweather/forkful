---
title:                "TypeScript: Generering av slumpmässiga tal"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

När man programmerar i TypeScript eller något annat programmeringsspråk, finns det många gånger behov att generera slumpmässiga nummer. Det kan vara för att skapa en mer dynamisk användarupplevelse, skapa unika identifierare eller helt enkelt för att testa en funktion. Oavsett anledning är det viktigt att veta hur man genererar slumpmässiga nummer på rätt sätt.

## Så här gör du

För att generera slumpmässiga nummer i TypeScript, finns det en inbyggd funktion som heter `Math.random()`. Denna funktion returnerar ett decimaltal mellan 0 och 1. Om man vill ha ett heltal istället, kan man använda `Math.floor()` för att avrunda talet nedåt. Om man vill ha ett heltal mellan ett visst intervall, måste man multiplicera resultatet från `Math.random()` med det önskade intervallet och sedan använda `Math.floor()` för att avrunda talet.

```TypeScript
// Genererar ett slumpmässigt decimaltal mellan 0 och 1
let randomDecimal = Math.random();

// Genererar ett slumpmässigt heltal mellan 0 och 10
let randomInteger = Math.floor(Math.random() * 10);

// Genererar ett slumpmässigt heltal mellan 5 och 15
let randomRange = Math.floor(Math.random() * 10) + 5;

console.log(randomDecimal); // Exempel output: 0.724871925567936 
console.log(randomInteger); // Exempel output: 7
console.log(randomRange); // Exempel output: 12
```

Om man vill ha mer kontroll över resultatet av det slumpmässiga numret, kan man använda sig av en seed. En seed är ett startvärde som tillsammans med algoritmen i `Math.random()` skapar ett förutbestämt mönster av slumpmässiga nummer. Detta är användbart om man t.ex. vill återskapa ett slumpmässigt nummer som användes tidigare. För att använda en seed måste man använda `Math.seedrandom()` från biblioteket "seedrandom".

```TypeScript
import seedrandom from 'seedrandom';

// Skapar en seed med texten "random"
let randomSeed = seedrandom("random");

// Genererar ett slumpmässigt decimaltal med seeden
let randomDecimal = randomSeed();

console.log(randomDecimal); // Exempel output: 0.1739793335947135
```

## Djupdykning

Generering av slumpmässiga nummer är en viktig del av många algoritmer och program. En vanlig missuppfattning är att `Math.random()` genererar helt slumpmässiga nummer. Men i verkligheten använder funktionen en algoritm baserad på ett startvärde för att producera en följd av pseudo-slumpmässiga nummer. Detta är varför det är viktigt att använda en seed om man vill återskapa ett slumpmässigt nummer.

Det finns många olika sätt att skapa ett slumptal i programmering och vilken metod man ska använda beror på vad man vill använda det till. Det är också viktigt att förstå att slumptalen inte är helt slumpmässiga och att det finns sätt att förutsäga nästa slumptal baserat på tidigare nummer i följden.

## Se även

- [Math.random() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [seedrandom - npm](https://www.npmjs.com/package/seedrandom)
- [The trouble with probability: Maths and random number generation - EX²OUTLIER](https://ex2outlier.com/the-trouble-with-probability-maths-and-random-number-generation/)