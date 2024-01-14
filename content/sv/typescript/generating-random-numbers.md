---
title:                "TypeScript: Generera slumpmässiga siffror"
simple_title:         "Generera slumpmässiga siffror"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig praxis inom programmering för att skapa variation och slumpmässighet i ett program. Det är särskilt användbart när man behöver simulera verkliga situationer eller testa olika scenarier.

## Hur man gör det

För att generera slumpmässiga nummer i TypeScript, kan vi använda oss av Math-funktionen. Vi kan använda metoden "random()" för att få ett nummer mellan 0 och 1, och sedan använda det för att skapa ett större intervall.

```TypeScript
// Generera ett slumpmässigt heltal mellan 0 och 10
let randomNum = Math.floor(Math.random() * 10);

// Generera ett slumpmässigt decimaltal mellan 0 och 100
let randomDecimal = Math.random() * 100;

// Generera ett slumpmässigt heltal mellan två givna nummer
function randomIntBetween(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Använda funktionen för att generera ett slumpmässigt nummer mellan 5 och 10
let randomNumber = randomIntBetween(5, 10);

console.log(randomNum); // Till exempel: 7
console.log(randomDecimal); // Till exempel: 58.352
console.log(randomNumber); // Till exempel: 8
```

## Djupdykning

För att förstå hur Math.random() fungerar, kan vi titta på dess implementering i JavaScript. Funktionen returnerar en pseudo-slumpmässig decimal mellan 0 och 1 genom att multiplicera ett tal med en stor multiplikator och sedan ta modulus av det. Multiplikatorn genereras av implementationen av JavaScript och är baserad på tiden och datorspecifika faktorer.

Det finns också andra metoder för att generera slumpmässiga nummer i TypeScript, som att använda sig av tredjepartsbibliotek som "random-js" eller "random-pick". Dessa ger mer avancerade metoder för att skapa önskade variationer och kontroll över nummerna.

## Se även

- [Math-funktionen på Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Random-js biblioteket](https://github.com/ckknight/random-js)
- [Random-pick biblioteket](https://www.npmjs.com/package/random-pick)