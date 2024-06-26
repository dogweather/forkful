---
date: 2024-01-20 17:47:39.764549-07:00
description: "Hur man g\xF6r: Historiskt sett har `.length`-egenskapen alltid varit\
  \ det prim\xE4ra s\xE4ttet att f\xE5 reda p\xE5 hur m\xE5nga tecken en str\xE4ng\
  \ inneh\xE5ller i JavaScript.\u2026"
lastmod: '2024-04-05T21:53:39.621913-06:00'
model: gpt-4-1106-preview
summary: "Historiskt sett har `.length`-egenskapen alltid varit det prim\xE4ra s\xE4\
  ttet att f\xE5 reda p\xE5 hur m\xE5nga tecken en str\xE4ng inneh\xE5ller i JavaScript."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Hur man gör:
```javascript
let greeting = "Hej världen!";
console.log(greeting.length);  // Output: 12

let emptyString = "";
console.log(emptyString.length); // Output: 0

let emojiString = "🙂🙃";
console.log(emojiString.length); // Output: 4 (emoji tar två tecken vardera)
```

## Djupdykning:
Historiskt sett har `.length`-egenskapen alltid varit det primära sättet att få reda på hur många tecken en sträng innehåller i JavaScript. Trots att detta koncept är rakt på sak, finner man intressanta implementationer när det kommer till unicode-tecken, som emojis, där varje "tecken" faktiskt kan bestå av flera underliggande kodenheter.

Det finns alternativ till att använda `.length`, såsom att loopa igenom en sträng och räkna tecken manuellt, men metoden är onödig och ineffektiv jämfört med den inbyggda egenskapen. I modern JavaScript, när man hanterar olika tecken, inklusive de som inte rymms inom det traditionella UCS-2 teckenområdet, kan vi använda `Array.from()` eller `[...str]` spridningsoperatorn för att skapa en array innan vi får dess längd, för att korrekt hantera tecken som emojis.

```javascript
let fancyString = "👩‍🚀🚀";
console.log(fancyString.length);           // Output: 5
console.log(Array.from(fancyString).length); // Output: 2
```

Notera att metoden med `Array.from()` är att föredra för att få en riktig räkning av tecken som representeras av surrogatpar i Unicode.

## Se även:
- MDN Web Docs om Strings: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
- Artikel om JavaScript och Unicode: https://dmitripavlutin.com/what-every-javascript-developer-should-know-about-unicode/
- ECMAScript specifikationen: http://www.ecma-international.org/ecma-262/
