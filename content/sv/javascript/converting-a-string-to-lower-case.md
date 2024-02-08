---
title:                "Konvertera en sträng till gemener"
aliases:
- sv/javascript/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:54.468711-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att konvertera en sträng till gemener innebär att omvandla alla bokstäver i strängen till små bokstäver. Programmerare gör detta för att standardisera textdata, till exempel för att underlätta jämförelser eller sökningar oavsett hur datan matades in.

## How to: (Hur gör man:)
```javascript
let greeting = "Hej Världen!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting);  // Output: "hej världen!"
```

```javascript
let cityName = "STOCKHOLM";
console.log(cityName.toLowerCase());  // Output: "stockholm"
```

```javascript
let mixedCase = "HeLsInGbOrG";
console.log(mixedCase.toLowerCase());  // Output: "helsingborg"
```

## Deep Dive (Djupdykning)
Innan JavaScript, på webbens tidiga dagar, hanterades text oftast som den matades in. Detta ledde till problem med datakonsistens. JavaScript introducerade `toLowerCase()` som en lösning på detta problem. 

Alternativ till `toLowerCase()` inkluderar t.ex. `toLocaleLowerCase()`, vilket tar hänsyn till användarens språkinställningar – användbart om du måste hantera speciella karaktärer som är unika för ett språk.

När `toLowerCase()` anropas går JavaScript igenom varje tecken i strängen och använder Unicode-mappningar för att byta ut stora bokstäver mot motsvarande små bokstäver. Detta betyder att konverteringen är pålitlig över olika språk och teckenuppsättningar.

## See Also (Se även)
- MDN Web Docs för `String.prototype.toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- MDN Web Docs för `String.prototype.toLocaleLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase
- En guide till JavaScripts Unicode-stöd: https://flaviocopes.com/javascript-unicode/
