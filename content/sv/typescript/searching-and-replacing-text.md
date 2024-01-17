---
title:                "Sökning och ersättning av text"
html_title:           "TypeScript: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text är en vanlig uppgift för programmerare. Det innebär att man upphittar en specifik bit av text och ersätter den med en annan. Detta kan vara användbart för att uppdatera felaktig information eller för att göra ändringar i koden för att förbättra dess funktionalitet.

## Hur man:
I TypeScript finns flera olika sätt att söka och ersätta text. Ett vanligt sätt är att använda metoden ```replace()```, där man anger den ursprungliga texten som ska ersättas och den nya texten. En annan metod är ```replaceAll()```, som ersätter alla förekomster av den ursprungliga texten med den nya texten.

Exempel:
```TypeScript
let text = "Hej världen!"
let newText = text.replace("världen", "allihopa")
console.log(newText) // Output: "Hej allihopa!"
```

## Fördjupning:
Att söka och ersätta text har funnits i programmering sedan tidigt 1960-tal, då det först implementerades i språket SNOBOL. I TypeScript finns det flera olika metoder för att utföra denna uppgift, varav vissa har olika prestanda och funktionalitet. Alternativ till ```replace()``` och ```replaceAll()``` är bland annat ```search()```, ```replaceAllWith()``` och ```splitJoin()```.

Det finns också andra språk och bibliotek som erbjuder inbyggda funktioner för att söka och ersätta text, som till exempel JavaScript och jQuery. För mer detaljerade implementationer i TypeScript rekommenderas att läsa dokumentationen för det aktuella språket eller biblioteket.

## Se även:
Dokumentation för ```replace()```: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace

Dokumentation för ```replaceAll()```: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll

Dokumentation för andra metoder för att söka och ersätta text i TypeScript: https://www.typescriptlang.org/docs/handbook/strings.html#searching-for-text-within-a-string