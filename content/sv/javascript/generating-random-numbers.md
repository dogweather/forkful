---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumptal är en process för att skapa nummer som inte kan förutsägas bättre än av ren slump. Programmerare gör detta för att skapa slumpmässighet i sina program, vilket är användbart i spel, simuleringar och testning.

## Hur man gör:

Du kan generera ett slumptal i Javascript genom att använda Math.random-funktionen. Här är ett exempel:

``` Javascript
var slumpNummer = Math.random();
console.log(slumpNummer);
```

Detta kommer att spotta ut ett slumptal mellan 0 (inkluderat) och 1 (exkluderat). Till exempel:

``` Javascript
0.23710482585866
```

Du kan även få ett slumptal mellan två specifika värden, som så:

``` Javascript 
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
console.log(getRandomInt(5, 15));
```

Detta kan producera ett nummer mellan 5 och 15:

``` Javascript
7
```

## Djupdykning:

Att generera slumptal är ett gammalt matematiskt och datavetenskapligt problem. Historiskt sett, förlitade sig tidig datorprogrammering på externa slumpgeneratorer, som kastning av tärningar eller radioaktivt nedfall.

Det finns andra sätt att generera slumptal i Javascript, till exempel förlita sig på tredjepartsbibliotek som `lodash` eller `chance`. 

Det är viktigt att notera att Math.random() inte genererar kryptografiskt säkra slumptal. Om du behöver kryptografiskt säkra slumptal bör du istället använda funktionen `window.crypto.getRandomValues()`.

## Se även:

1. [MDN Web Docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
3. [NPM: Lodash](https://www.npmjs.com/package/lodash), [NPM: Chance](https://www.npmjs.com/package/chance)
4. [Säkerhet.stackexchange: Använda Math.random() för kryptografiska ändamål](https://security.stackexchange.com/questions/14357/is-javascripts-math-random-cryptographically-secure)