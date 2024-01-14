---
title:                "TypeScript: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är ett viktigt steg inom programmering eftersom det gör det möjligt för användare att förstå och läsa datum i ett önskat format. Det kan också vara användbart för att spara och överföra datumdata mellan olika system.

## Så här gör du

Här kommer vi att visa dig hur du konverterar ett datum till en sträng i TypeScript med hjälp av några enkla kodexempel. Observera att dessa exempel endast är avsedda för läsbarhet och att de kan anpassas efter dina specifika behov.

```Typescript
// Skapa ett datumobjekt med aktuellt datum och tid
let datum = new Date();

// Konvertera till en sträng i formatet "dd/mm/åååå"
console.log(datum.toLocaleDateString("sv-SE")); // Output: "09/03/2021"

// Konvertera till en sträng i formatet "mm/dd/åååå"
console.log(datum.toLocaleDateString("en-US")); // Output: "03/09/2021"

// Konvertera till en sträng i formatet "åååå-mm-dd"
console.log(datum.toISOString()); // Output: "2021-03-09T13:14:43.454Z"
```

Som du kan se i kodexemplen ovan kan du använda `toLocaleDateString()` för att formatera datumet efter språk och regioninställningar. Om du vill ha en mer standardiserad ISO-datumssträng kan du använda `toISOString()`.

## Utökad information

För att förstå den fullständiga processen för att konvertera ett datum till en sträng i TypeScript, är det viktigt att förstå hur datumobjekt hanteras i språket. I TypeScript finns det två huvudsakliga typer av datumobjekt: `Date` och `DateTime`.

Date-objektet representerar ett datum genom att kombinera årtal, månad och dagar. DateTime-objektet å andra sidan innehåller även information om tid och tidszon.

För att konvertera ett datum till en sträng måste du först skapa ett datumobjekt med den önskade informationen och sedan använda en av de inbyggda funktionerna som `toLocaleDateString()` eller `toISOString()`.

## Se även

- [MDN webbplats - Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript dokumentation - Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#dates)