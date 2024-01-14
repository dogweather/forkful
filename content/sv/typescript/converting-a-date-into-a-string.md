---
title:                "TypeScript: Omvandling av ett datum till en sträng"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera en datum till en sträng är en viktig färdighet för alla TypeScript-programmerare. Det ger möjlighet att visa datum på ett mer läs-vänligt sätt och att göra det enklare att hantera datum i din kod.

## Hur man gör det

```TypeScript
const datum = new Date("2021-09-15");
const datumString = datum.toDateString();
console.log(datumString); // Output: Wed Sep 15 2021
```

Att konvertera ett datum till en sträng är relativt enkelt med hjälp av inbyggda metoder i JavaScripts Date-objekt. Först måste du skapa ett Date-objekt med ett specifikt datum, antingen genom att ange datumet som en string eller med hjälp av olika metoder som `getDay()` och `getFullYear()`. Sedan kan du använda `toDateString()` för att få ett läs-vänligt format för datumet.

```TypeScript
const datum = new Date();
const dag = datum.getDay(); // Returnerar dagen i veckan som ett nummer
const manad = datum.getMonth(); // Returnerar månaden som ett nummer
const ar = datum.getFullYear(); // Returnerar året som ett fyrsiffrigt nummer

const datumString = dag + "/" + manad + "/" + ar; // Format: DD/MM/YYYY
console.log(datumString); // Output: 15/9/2021
```

För att skapa ett mer specifikt format kan du använda andra inbyggda metoder som `getHours()`, `getMinutes()` och `getSeconds()` för att inkludera tiden i din sträng.

## Djupdykning
När du konverterar ett datum till en sträng kan det vara användbart att känna till några olika format-alternativ. Ett sätt är att använda den globala `Intl`-modulen, som ger möjlighet att skapa datum och tid på olika språk och format.

```TypeScript
const datum = new Date();
const lokalTid = new Intl.DateTimeFormat("sv-SE").format(datum);
console.log(lokalTid); // Output: 15/9/2021
```

Du kan också använda metoder som `getDate()`, `getMonth()` och `getFullYear()` tillsammans med `toLocaleDateString()` för att skapa en sträng baserat på olika lokala inställningar.

```TypeScript
const datum = new Date();
const dag = datum.getDate(); // Returnerar dagen i månaden
const manad = datum.getMonth() + 1; // Returnerar månaden som ett nummer
const ar = datum.getFullYear(); // Returnerar året som ett fyrsiffrigt nummer

const lokalTid = new Intl.DateTimeFormat("sv-SE").format(datum);
const datumString = dag + "/" + manad + "/" + ar; // Format: DD/MM/YYYY
console.log(datumString); // Output: 15/9/2021
```

## Se också
- [MDN web docs: Datum API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN web docs: Intl.DateTimeFormat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)
- [Date-fns - Ännu en JS datum-bibliotek](https://date-fns.org/)