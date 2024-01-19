---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generera Slumptal i TypeScript

## Vad & Varför?
Slumptalsgenerering handlar om att skapa tal som inte kan förutsägas, vilket är viktigt i många programmeringsuppgifter som spel, kryptering och modellering av slumpmässiga händelser.

## Hur man gör:
Här är ett enkelt exempel på hur man genererar ett slumptal mellan 0 och 1 i TypeScript:

```TypeScript 
let slump = Math.random();
console.log(slump);
```

Och för att få ett heltal mellan två gränser, använder vi följande kod:

``` TypeScript 
function faSlumptal(min: number, max: number): number {
  let slump = Math.floor(Math.random() * (max - min + 1)) + min;
  return slump;
}
console.log(faSlumptal(1, 10));
```

## Djupdykning
Den historiska bakgrunden till slumptalsgenerering sträcker sig tillbaka till de tidiga dagarna för datorer. På den tiden var maskinvaredongligt verkligen slumpmässiga, men i moderna datorer genereras "slumpmässiga" tal algoritmiskt och de kallas för pseudoslumpmässiga tal.

Det finns andra sätt att generera slumpmässiga nummer i TypeScript, till exempel genom att använda andra bibliotek som crypto eller uuid för att generera unika ID.

Huvuddelen av `Math.random()`-funktionen i JavaScript (som TypeScript bygger på) är en implementation av en variant av Mersenne Twister, en välkänd algoritm för pseudoslumpmässiga tal.

## Se även 
Kolla in följande källor för mer information och alternativa tillvägagångssätt:

- [MDN Web Docs on Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Node.js crypto module documentation](https://nodejs.org/api/crypto.html)
- [Understanding UUIDs](https://www.sohamkamani.com/blog/uuid-versions-explained/)