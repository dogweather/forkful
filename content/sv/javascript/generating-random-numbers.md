---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:31.324458-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal handlar om att skapa ett tal som inte är förutsägbart. Programmerare gör det för att simulera chansprocesser, testa algoritmer under varierande förhållanden eller till exempel tilldela unika ID:n.

## Hur gör man:
Du kan använda `Math.random()` för att generera ett basalt slumpmässigt tal mellan 0 (inkluderat) och 1 (exkluderat). Så här kör du:

```Javascript
// Skapar ett grundläggande slumpmässigt tal:
let randomNum = Math.random();
console.log(randomNum);

// För ett tal mellan 0 och 10:
let randomNumToTen = Math.floor(Math.random() * 10);
console.log(randomNumToTen);

// För ett tal mellan 1 och 100:
let randomNumOneToHundred = Math.floor(Math.random() * 100) + 1;
console.log(randomNumOneToHundred);
```
Exempelutdata:
```
0.439857376102
4
57
```

## Djupdykning:
Slumpmässiga nummergenerering har en lång historia och är avgörande för kryptografi. JavaScripts `Math.random()` är egentligen inte helt slumpmässig, utan pseudoslumpmässig – tillräckligt för många ändamål men inte för kryptografisk säkerhet. Vill du ha kryptografisk säkerhet, använd `crypto.getRandomValues()`. Det kan vara intressant att veta att algoritmen bakom `Math.random()` kan variera mellan olika JavaScript-motorer, vilket betyder att dess resultat inte är helt konsekventa över olika plattformar.

## Se även:
- MDN Web Docs för `Math.random()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- MDN Web Docs för `crypto.getRandomValues()`: https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
- En djupgående artikel om slumpmässighet och dess användning i datorprogrammering från 2ality: http://2ality.com/2017/05/seeding-rng.html
