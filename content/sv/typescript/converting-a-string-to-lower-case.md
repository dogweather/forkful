---
title:                "TypeScript: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Varför konvertera en sträng till gemener?

Att konvertera en sträng till gemener är en vanlig uppgift inom programmering, särskilt när man arbetar med användarinput eller jämför strängar. Genom att omvandla en sträng till gemener ser vi till att all text är i samma format, vilket kan underlätta vid jämförelser och sökningar. I denna bloggpost kommer jag att visa hur du enkelt kan konvertera en sträng till gemener i TypeScript.

## Så här gör du

För att konvertera en sträng till gemener i TypeScript kan vi använda den inbyggda metoden "toLowerCase". Denna metod tar emot en sträng och returnerar en ny sträng med alla bokstäver konverterade till gemener. Nedan finns en kodexempel på hur du kan använda denna metod:

```TypeScript
let sträng = "HEJ DÄR!";
let gemener = sträng.toLowerCase();
console.log(gemener);

// Output: hej där!
```

Vi tilldelar variabeln "sträng" värdet "HEJ DÄR!" och sedan tilldelar vi en ny variabel "gemener" som innehåller det konverterade värdet av "sträng" med hjälp av "toLowerCase" metoden. Slutligen skriver vi ut "gemener" till konsolen och får outputen "hej där!".

## Djupdykning

Förutom att konvertera bokstäver till gemener har "toLowerCase" metoden också möjlighet att hantera specialtecken och diakritiska tecken. Detta innebär att om du har en sträng som innehåller exempelvis "Å", "Ä" eller "Ö", så kommer de också att konverteras till gemener.

Det är också viktigt att notera att "toLowerCase" endast konverterar bokstäver till gemener och inte påverkar andra tecken, såsom siffror eller specialtecken. Dessa kommer fortfarande vara kvar i strängen efter konverteringen.

## Se även

Här är några användbara länkar för dig som vill lära dig mer om konvertering av strängar till gemener i TypeScript:

- [MDN webbdocs: DOMString.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript Handbook: String Operations](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string-operations)
- [codecademy: How to Use (and Abuse) Lowercase](https://www.codecademy.com/articles/javascript-uppercase-lowercase)

# Se också

För mer information om andra programmeringsrelaterade ämnen på svenska, besök gärna [Programming Blogg på Acodez](https://www.acodez.se/blog).