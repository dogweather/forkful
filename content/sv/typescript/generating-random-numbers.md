---
title:                "Generering av slumpmässiga tal"
html_title:           "TypeScript: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig uppgift inom programmering, speciellt inom spel eller simuleringar. Det är användbart för att skapa variation och slumpmässighet inom programmet, vilket kan göra det mer underhållande eller realistiskt.

## Så här gör du

För att generera slumpmässiga nummer i TypeScript använder man Math.random() funktionen. Detta returnerar ett tal mellan 0 och 1, inklusive 0 men exklusive 1. För att få ett större intervall, kan man multiplicera resultatet med det önskade antalet och sedan addera ett, som i exemplet nedan:

```typescript
// Generera ett slumpmässigt heltal mellan 1 och 100
let randomNumber = Math.floor(Math.random() * 100) + 1;
console.log(randomNumber); // output: ett tal mellan 1 och 100
```

Om man vill ha ett annat intervall kan man justera värdet som den slumpmässiga funktionen multipliceras och adderas med. Till exempel, om man vill ha ett tal mellan 50 och 100 kan man multiplicera med 50 och sedan addera 50 istället.

```typescript
// Generera ett slumpmässigt heltal mellan 50 och 100
let randomNumber = Math.floor(Math.random() * 50) + 50;
console.log(randomNumber); // output: ett tal mellan 50 och 100
```

För att generera slumpmässiga flyttal kan man använda Math.random() direkt som i följande exempel:

```typescript
// Generera ett slumpmässigt flyttal mellan 0 och 1
let randomNumber = Math.random();
console.log(randomNumber); // output: ett tal mellan 0 och 1
```

## Djupdykning

För att förstå hur Math.random() fungerar mer i detalj kan man titta på dess implementation. Det är en pseudoslumpmässig funktion, vilket betyder att den egentligen inte genererar verkligt slumpmässiga tal utan använder en matematisk formel baserad på en initial "seed" eller startpunkt. Denna seed är vanligtvis baserad på aktuell tid, vilket gör resultaten mer slumpmässiga.

Trots att funktionen inte är helt slumpmässig, ger den tillräckligt realistiska resultat för de flesta användningssyften.

## Se även

- [Math.random() i MDN webbdokumentation](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Pseudoslumpmässiga tal i Wikipedia](https://sv.wikipedia.org/wiki/Pseudoslumpm%C3%A4ssiga_tal)
- [Slumpgenerator i programmeringsspråk på rosettacode.org](https://rosettacode.org/wiki/Category:Random_number_generation)