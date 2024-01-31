---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:48:33.764544-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Längden på en sträng är antalet tecken den innehåller. Programmerare måste ofta veta detta för att validera inmatning, skapa substrängar eller jämföra strängarnas storlekar.

## Hur man gör:
```typescript
let greeting: string = 'Hej världen!';
let length: number = greeting.length;
console.log(length); // Output: 12
```

För att räkna utrymmen och specialtecken, används de precis som andra tecken:
```typescript
let stringWithSpaces: string = 'Hej världen, hur mår du?';
console.log(stringWithSpaces.length); // Output: 27

let stringWithEmoji: string = 'Hej 👋';
console.log(stringWithEmoji.length); // Output: 5 (emoji counts as 2 characters)
```

## Djupdykning
Längden på en sträng har varit en grundläggande egenskap i de flesta programmeringsspråk sedan början. I TypeScript, som är en överbyggnad på JavaScript, tilldelas egenskapen `.length` till en sträng för att få detta värde. Det är viktigt att notera att i UTF-16 (som TypeScript använder), så kan enstaka tecken, som emojis, räknas som två 'units', vilket kanske inte alltid matchar det förväntade antalet synliga tecken.

### Alternativ
Du kan också använda loopar eller andra funktioner för att manuellt räkna tecken, men det är oftast onödigt och ineffektivt. Till exempel kan du använda `Array.from` för att skapa en korrekt längd för strängar med emojis:

```typescript
let correctLengthEmoji: string = 'Hej 👋';
console.log(Array.from(correctLengthEmoji).length); // Output: 4
```

### Implementationsdetaljer
`.length`-egenskapen är snabb och effektiv eftersom den är en fördefinierad egenskap på strängobjektet i JavaScripts motor. Den räknar 'code units' snarare än 'code points', vilket kan bli missvisande för vissa Unicode-tecken. För aktuella applikationer är det bra att förstå skillnaden mellan dessa, speciellt när det kommer till internationalisering och tecken som ligger utanför den grundläggande multilingual plane (BMP).

## Se även
- MDN Web Docs om `.length` egenskapen för strängar: [MDN: String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript Handbook om strängtyper: [TypeScript: Handbook - String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- Artikel om Unicode och JavaScript-strängar: [Handling Unicode in JavaScript](https://flaviocopes.com/javascript-unicode/)
