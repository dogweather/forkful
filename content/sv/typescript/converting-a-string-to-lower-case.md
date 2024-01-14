---
title:    "TypeScript: Omvandla en sträng till gemener"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är en vanlig uppgift inom programmering, särskilt vid hantering av användarens inmatning. Genom att omvandla en sträng till gemener kan vi garantera att inmatningen är enhetlig och enklare att behandla.

## Så här gör du

Vi kan enkelt konvertera en sträng till gemener med hjälp av inbyggda metoderna i TypeScript. Nedan följer ett exempel på hur vi kan göra detta:

```TypeScript
let namn = "JOHANNA";
console.log(namn.toLowerCase()); // ut skrift: johanna
```

I detta exempel använder vi metoden `toLowerCase()` på variabeln `namn` för att konvertera den till gemener. Detta fungerar för alla typer av strängar, oavsett hur de är skrivna.

## Djupdykning

En intressant aspekt av att konvertera en sträng till gemener är att det inte bara handlar om att ändra bokstävernas storlek. Vissa språk, som svenska, har även bokstäver med accenter som kan påverkas av denna konvertering. Om du vill behålla dessa accenter i strängen kan du använda metoden `toLocaleLowerCase()` istället. Denna metod tar hänsyn till språket i strängen och behåller eventuella accenter.

En annan sak att tänka på är att om du vill konvertera en sträng som innehåller siffror, så kommer dessa inte att påverkas av metoden `toLowerCase()`. De kommer fortfarande att behålla sin ursprungliga form. Om du vill inkludera även siffror i konverteringen kan du använda metoden `replace()` tillsammans med en reguljär expression.

## Se även

- [String.toLowerCase() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [String.replace() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [String.toLocaleLowerCase() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)