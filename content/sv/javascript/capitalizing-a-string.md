---
title:                "Javascript: Stor bokstavering av en sträng"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna omvandla en sträng till versaler är en viktig del av programmering, särskilt när man arbetar med textbearbetning och formatering av utdata. Det gör att texten blir lättare att läsa och kan också krävas för att uppfylla vissa krav för vissa program eller webbsidor. Det är en enkel men användbar funktion som alla utvecklare bör ha i sin verktygslåda.

## Hur man gör det

Det finns många olika sätt att omvandla en sträng till versaler i Javascript, men det vanligaste och enklaste är att använda funktionen `toUpperCase()`. Denna funktion tar en sträng som argument och returnerar en ny sträng med bara versaler. Här är ett exempel på hur du kan använda det i en kod:

```Javascript
let text = "hej! välkommen till min blogg!";
console.log(text.toUpperCase());

//Output: HEJ! VÄLKOMMEN TILL MIN BLOGG!
```

Som du kan se behåller funktionen `toUpperCase()` alla andra teckenbaserade delar av strängen, men omvandlar bara de bokstäver som är i gemener till versaler.

Det är också möjligt att använda `charAt()` och `charCodeAt()` för att iterera igenom en sträng och omvandla varje tecken till versaler, men detta är ett mer komplicerat tillvägagångssätt och rekommenderas bara för avancerade användare.

## Deep Dive

För de som vill lära sig mer om hur versaler fungerar i Javascript och andra programmeringsspråk kan det vara värt att titta närmare på Unicode-standarderna. Versalt till gemena omvandling är en del av Unicode-tablellen och är baserad på deras hierarkiska struktur av bokstäver och tecken. Detta är en intressant djupdykning i hur datorer hanterar text och kan hjälpa till att förbättra dina färdigheter som utvecklare.

## Se även

- [String.prototype.toUpperCase() - MDN Web Docs](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [JavaScript Strings - W3Schools](https://www.w3schools.com/js/js_strings.asp)
- [Unicode Character Categories - Unicode.org](https://unicode.org/versions/Unicode13.0.0/ch04.pdf)