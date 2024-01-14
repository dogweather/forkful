---
title:                "Javascript: Omvandla en sträng till gemener"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener är en vanlig uppgift när man arbetar med Javascript. Det kan komma till nytta när man behöver jämföra strängar eller när man vill justera användarinput för att säkerställa konsistens.

## Hur man gör det
Du kan enkelt konvertera en sträng till gemener genom att använda metoden `toLowerCase()`. Se nedan för ett exempel:
```Javascript
var str = "Hej Världen!";
var lowerStr = str.toLowerCase();
console.log(lowerStr);
// Output: "hej världen!"
```

Du kan också använda `toLowerCase()` tillsammans med en variabel, som i exemplet nedan:
```Javascript
var name = "Anna";
var lowerName = name.toLowerCase();
console.log("Hej, " + lowerName + "!");
// Output: "Hej, anna!"
```

## En djupdykning
När du använder `toLowerCase()` skapar du en ny sträng som innehåller samma tecken som den ursprungliga strängen, men alla bokstäver är omvandlade till gemener. Detta kan vara användbart när du vill ignorera skillnader i storlek när du jämför strängar.

## Se också
- [Javascript String toLowerCase() metod](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [String.prototype.toLowerCase() MDN-dokumentation](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)