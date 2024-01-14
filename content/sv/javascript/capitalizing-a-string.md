---
title:                "Javascript: Kapitalisera en sträng"
simple_title:         "Kapitalisera en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna ändra en sträng till versaler eller gemener är en viktig funktion i programmering eftersom det ger en möjlighet att hantera text på ett mer effektivt sätt. Till exempel kan man använda detta för att jämföra strängar på ett mer enhetligt sätt.

## Hur Du Gör Det

För att ändra en sträng till versaler i Javascript kan du använda metoden `toUpperCase()`. Här är ett exempel på hur du kan använda denna metod:

```Javascript
let str = "hej allesammans!";
let capitalizedStr = str.toUpperCase();

console.log(capitalizedStr);
```

Detta kommer att skriva ut "HEJ ALLESAMMANS!" i din konsol. Du kan också använda metoden `toLowerCase()` för att ändra en sträng till gemener på samma sätt.

## Djupdykning

Det finns flera olika sätt att ändra en sträng till versaler eller gemener i Javascript. En annan metod är att använda `charAt()` och `charCodeAt()` för att iterera genom varje tecken i strängen och ändra dess kodvärde till det önskade fallet. Detta är dock en något mer avancerad metod och används sällan jämfört med `toUpperCase()` och `toLowerCase()`.

Det är också viktigt att komma ihåg att det finns skillnader i hur teckenkoderna för versaler och gemener är ordnade i olika teckenuppsättningar, vilket kan orsaka problem när du hanterar text på flera språk. Det är därför viktigt att alltid kontrollera vilken teckenuppsättning som används för att undvika oväntade resultat.

## Se Också

- [Doc: String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Doc: String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Blogg: Versaler och gemener i Unicode](https://unicode.org/faq/casemap_charprop.html)