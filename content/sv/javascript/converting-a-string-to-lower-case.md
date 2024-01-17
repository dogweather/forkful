---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "Javascript: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Konvertering av en sträng till gemener är en vanlig operation inom programmering. Det innebär att alla bokstäver i en sträng ändras till små bokstäver. Detta görs ofta för att underlätta jämförelser mellan olika strängar och för att följa konventionen att variabelnamn i Javascript ska skrivas i camelCase.

## Hur gör man:
För att konvertera en sträng till gemener i Javascript kan man använda sig av metoden .toLowerCase(). Här är ett exempel på hur man kan göra det:

```Javascript
let str = "HeJ GöR IKrilL"
console.log(str.toLowerCase());
```
Output: "hej gör ikkrill"

Notera att det ursprungliga värdet av strängen inte ändras, vi använder bara en funktion för att skapa en tillfällig ny sträng med alla bokstäver i gemener.

## Djupdykning:
Konvertering av strängar till gemener är inte en ny funktion, det har funnits sedan de första programmeringsspråken. Men i vissa språk, som till exempel C, fanns det inte inbyggda metoder för detta och man behövde använda sig av så kallade ASCII tabeller för att göra konverteringen. Javascript, liksom många moderna programmeringsspråk, har inbyggda metoder för att underlätta denna process.

En alternativ metod för att konvertera en sträng till gemener är att loopa genom alla bokstäver och ändra dem manuellt. Detta kan vara användbart för programmerare som vill ha mer kontroll över sin kod, men det kräver mer kod och kan vara mer tidskrävande.

När det kommer till implementationen av .toLowerCase() i Javascript så använder det sig av Unicode-teckenuppsättningen för att konvertera bokstäverna. Detta innebär att även tecken från andra språk, som till exempel accentuerade bokstäver, kan konverteras till gemener på ett korrekt sätt.

## Se även:
- Javascript String toLowerCase() Method: https://www.w3schools.com/jsref/jsref_tolowercase.asp
- ASCII Table: https://www.asciitable.com/
- Unicode: https://unicode-table.com/en/