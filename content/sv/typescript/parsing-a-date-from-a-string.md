---
title:                "Analysera ett datum från en sträng"
html_title:           "TypeScript: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa en datumsträng innebär att konvertera en sträng som innehåller information om ett datum till ett datumobjekt som kan manipuleras och användas i programmeringsspråket TypeScript. Programmerare utför denna åtgärd för att kunna utföra olika operatörer och beräkningar på datumet och för att kunna representera det på ett enhetligt sätt i deras program.

## Hur går man tillväga?
Att parsa en datumsträng i TypeScript är enkelt. Det finns inbyggda funktioner och metoder som gör detta möjligt. Nedan följer ett exempel på hur man parsar ett datum från en sträng och skriver ut det i konsolen:

```TypeScript
const dateStr = "2021-07-21";
const parsedDate = new Date(dateStr);
console.log(parsedDate);
```

Output: 2021-07-21T00:00:00.000Z

Här använder vi funktionen `new Date()` för att skapa ett datumobjekt från strängen. Denna funktion tar emot en sträng med datumet enligt formatet ÅR-MM-DD. Det är viktigt att notera att datumet är i UTC-tid vilket visas genom bokstaven Z i slutet av outputen. Om du vill representera datumet i din lokala tidszon måste du använda `toLocaleDateString()`-metoden.

## Djupdykning
Parsing av datum från strängar är en viktig aspekt av mjukvaruutveckling och används ofta för att hantera användarens input i form av datum. Det är också en del av den grundläggande funktionaliteten i de flesta programmeringsspråk och har funnits sedan de tidiga dagarna av datorprogrammering.

Det finns flera olika sätt att parsa datum från strängar i TypeScript, inklusive användandet av tredjepartsbibliotek och regex. Men den vanligaste och enklaste metoden är att använda `new Date()`-funktionen.

## Se även
- [Mozilla Developer Network: Date.parse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [W3Schools: Date.parse()](https://www.w3schools.com/jsref/jsref_parse.asp)