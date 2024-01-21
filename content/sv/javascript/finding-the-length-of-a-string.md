---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:39.764549-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
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