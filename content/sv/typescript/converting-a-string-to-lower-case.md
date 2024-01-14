---
title:                "TypeScript: Omvandla en sträng till små bokstäver"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener, eller lowercase, är ett vanligt problem som många programmerare står inför. Det kan vara användbart när du behöver göra jämförelser mellan strängar eller behöver formatera data på ett enhetligt sätt.

## Hur man gör det

För att konvertera en sträng till gemener i TypeScript, kan du använda metoden `toLowerCase()` som finns tillgänglig för strängar. Här är ett exempel på hur du kan använda den:

```TypeScript
let namn = "Sven";
console.log(namn.toLowerCase());
```

Output:

```
sven
```

Som du kan se konverterades den ursprungliga strängen "Sven" till en sträng med gemener "sven" genom att använda `toLowerCase()`.

## Djupdykning

När du använder `toLowerCase()` för att konvertera en sträng till gemener, kommer alla bokstäver i strängen att omvandlas, även specialtecken och siffror. Detta inkluderar också bokstäver med diakritiska tecken, som å, ä, ö. Det gör att du kan göra exakta jämförelser mellan strängar oavsett bokstävernas storlek eller diakritiska tecken.

Det är också värt att notera att `toLowerCase()` inte ändrar på den ursprungliga strängen, utan returnerar en ny sträng med den konverterade versionen. Så om du behöver behålla den ursprungliga strängen, se till att tilldela den konverterade versionen till en ny variabel.

## Se även

Här är några användbara länkar för att lära dig mer om strängar i TypeScript och andra sätt att hantera dem:

- [Officiell TypeScript dokumentation om strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [En guide till stränghantering i TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-typescript)
- [W3Schools artikel om strängmetoder i TypeScript](https://www.w3schools.com/js/js_string_methods.asp)

Tack för att du läser!