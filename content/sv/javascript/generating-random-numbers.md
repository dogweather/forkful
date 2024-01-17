---
title:                "Generering av slumpmässiga tal"
html_title:           "Javascript: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

Random tal är en viktig del av programmering, men vad är det egentligen och varför gör programmerare det? Låt oss ta en titt på hur man genererar slumpmässiga tal i Javascript och varför det är en nyckelfunktion för många program.

## Vad & Varför?

Att generera slumpmässiga tal är en viktig del av programmering eftersom det tillåter oss att skapa variation och representation av slumpmässig data i våra program. Det kan vara användbart för spel, simuleringar, kryptering och mycket mer. Genom att använda slumpmässiga tal blir våra program mer dynamiska och realistiska.

## How to:

Att generera slumpmässiga tal i Javascript är enkelt med hjälp av den inbyggda funktionen Math.random(). Denna funktion returnerar ett pseudo-slumpmässigt tal mellan 0 och 1. Om vi till exempel vill generera ett slumpmässigt tal mellan 1 och 10, kan vi använda följande kod:

```Javascript
let randomNum = Math.random(); // genererar ett tal mellan 0 och 1
randomNum = Math.floor(randomNum * 10) + 1; // multiplicera med 10 och lägg till 1 för att få ett tal mellan 1 och 10
console.log(randomNum); // output: ett slumpmässigt tal mellan 1 och 10
```

Vi kan också använda funktionen Math.floor() för att avrunda talet nedåt och få ett heltal. På så sätt kan vi generera ett slumpmässigt heltal mellan två specifika värden.

```Javascript
let randomNum = Math.floor(Math.random() * 10) + 5; // genererar ett heltal mellan 5 och 14
console.log(randomNum); // output: ett slumpmässigt tal mellan 5 och 14
```

## Deep Dive:

Det första sättet att generera slumpmässiga tal var genom att använda fysiska slumpgeneratorer som tärningar eller roulettehjul. Detta var långt innan datorer eller programmering existerade. Idag används pseudo-slumpmässiga tal, som genereras av algoritmer, eftersom de är snabbare och enklare att implementera.

För att få mer kontroll över genererade slumpmässiga tal, finns det även andra sätt att generera dem i Javascript. Till exempel genom att använda bibliotek som Seedrandom eller CryptoJS.

Seedrandom tillåter oss att skapa slumpmässiga tal som är beroende av en specifik kedja av tal, vilket gör det möjligt för oss att återupprepa en specifik sekvens av "slumpmässiga" tal. CryptoJS å andra sidan, genererar slumpmässiga tal baserat på sanna slumpmässiga händelser, som t.ex. tangenttryckningar eller musrörelser.

## See Also:

För mer information om hur man genererar slumpmässiga tal i Javascript, kolla in dessa resurser:

- [MDN web docs om Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Math.random() vs Crypto.getRandomValues() - Stack Overflow](https://stackoverflow.com/questions/45145215/math-random-vs-crypto-getrandomvalues-for-generating-secure-random-numbers)
- [Seedrandom biblioteket](https://github.com/davidbau/seedrandom)