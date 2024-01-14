---
title:    "Gleam: Generering av slumpmässiga nummer"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpade nummer är ett vanligt behov inom programmering, speciellt inom spel- och simuleringstillämpningar. Det kan också vara användbart för att skapa testdata eller för att räkna ut statistiska sannolikheter.

## Hur man gör det

För att generera slumpade nummer i Gleam behöver du importera modulen `random` och använda funktionen `int`, som tar emot två parameterar: ett minimum och ett maximum. Här är ett enkelt exempel på hur du kan använda det:

```Gleam
import random

let random_number = random.int(1, 100)

```

Detta kodblock kommer att ge ett slumpat heltal mellan 1 och 100. Om du vill ha ett decimaltal kan du istället använda funktionen `float`, som tar emot samma parametrar men returnerar ett flyttal istället.

Men vad händer om du vill ha en lista av slumpade nummer? För att göra det kan du använda funktionen `list`, som tar emot ett antal parametrar och returnerar en lista av slumpade nummer. Här är ett exempel:

```Gleam
import random

let random_list = random.list(10, 1, 100)

```

Detta kodblock kommer att returnera en lista med 10 slumpade heltal mellan 1 och 100. Du kan självklart ändra antalet nummer och intervallet för att anpassa det efter dina behov.

## Djupdykning

Bakom kulisserna använder Gleam en algoritm som kallas "Mersenne Twister" för att generera slumpade nummer. Denna algoritm är väl testad och anses vara en av de bästa metoderna för att skapa tillförlitliga slumpade nummer. Om du vill lära dig mer om hur den fungerar och vilka fördelar den har kan du läsa mer här: [Mersenne Twister på Wikipedia](https://en.wikipedia.org/wiki/Mersenne_Twister).

## Se även

- [Gleams officiella dokumentation för modulen `random`](https://gleam.run/modules/random.html)
- [Gleam Playground](https://gleam.run/try), där du kan testa olika variationer av kodexempelna ovan.

Tack för att du läste! Hoppas detta har hjälpt dig att förstå hur du kan generera slumpade nummer i Gleam. Lycka till med dina program!