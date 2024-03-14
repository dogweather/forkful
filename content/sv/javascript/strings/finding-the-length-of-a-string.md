---
date: 2024-01-20 17:47:39.764549-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken i den. Programmerare beh\xF6ver detta f\xF6r att validera indata, manipulera\
  \ text eller bara f\xF6r\u2026"
lastmod: '2024-03-13T22:44:38.284412-06:00'
model: gpt-4-1106-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken i den. Programmerare beh\xF6ver detta f\xF6r att validera indata, manipulera\
  \ text eller bara f\xF6r\u2026"
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Programmerare behöver detta för att validera indata, manipulera text eller bara för att hålla koll på hur mycket data de hanterar.

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
