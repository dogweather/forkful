---
title:                "Att göra en sträng stor bokstavsförsjuten"
html_title:           "TypeScript: Att göra en sträng stor bokstavsförsjuten"
simple_title:         "Att göra en sträng stor bokstavsförsjuten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför du skulle vilja ta dig tid att "capitalizing" en sträng, eller göra första bokstaven i varje ord i en sträng till en stor bokstav. Det finns faktiskt flera fördelar med att använda denna teknik i dina applikationer.

För det första, kan det förbättra läsbarheten och estetiken av din kod. Genom att använda stora bokstäver vid början av varje ord, blir det enklare att skilja mellan olika variabler och namn i din kod. Dessutom kan det hjälpa till att göra ditt kod till mer lättläst och professionellt utseende.

## Så här gör du

För att capitalizing en sträng i TypeScript, finns det flera olika sätt att åstadkomma detta. Här är två enkla sätt att göra det:

```TypeScript
// Använda inbyggd JavaScript metod
const str = "hej, detta är en exempelsträng";
const capitalizedStr = str.charAt(0).toUpperCase() + str.slice(1);
console.log(capitalizedStr);
// Output: "Hej, detta är en exempelsträng"

// Använda TypeScript inbyggd metod
const str = "hej, detta är en exempelsträng";
const capitalizedStr = str.replace(/\b\w/g, c => c.toUpperCase());
console.log(capitalizedStr);
// Output: "Hej, Detta Är En Exempelsträng"
```

Som du kan se, kan du antingen använda den inbyggda JavaScript-metoden "toUpperCase()" för att göra första bokstaven till en stor bokstav och sedan använda "slice()" för att återställa resten av strängen, eller så kan du använda TypeScript-metoden "replace()" med ett reguljärt uttryck för att samtidigt ändra alla första bokstäver i varje ord.

## Djupdykning

För dem som är intresserade av att förstå mer om hur capitalizing en sträng fungerar i bakgrunden, kan vi titta närmare på den inbyggda "replace()" metoden i TypeScript. Det reguljära uttrycket "\b\w" söker efter enbart de första bokstäverna i varje ord i strängen. När matchningen är hittad, ersätts det med en stora bokstav genom användning av pil notationen (=>), vilket är en av de snygga funktionerna i TypeScript. Denna process upprepas för varje matchning som hittas i strängen, vilket ger oss en "capitalized" version av strängen.

## Se också

* [TypeScript officiella hemsida](https://www.typescriptlang.org/)
* [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
* [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)