---
title:                "Omvandla ett datum till en sträng"
html_title:           "TypeScript: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
 Att konvertera ett datum till en sträng är när du ändrar formatet på datumet till en text representation. Programmerare gör det för att kunna visa datumet på ett förutbestämt sätt i sina applikationer eller för att hantera olika formateringar i olika länder.

## Så här gör du:
För att konvertera ett datum till en sträng i TypeScript kan du använda `toLocaleDateString()` metoden. Här är ett enkelt exempel för att konvertera dagens datum till en sträng i formatet "dd/mm/yyyy":

```TypeScript
const today = new Date();
const convertedDate = today.toLocaleDateString('sv-SE');
console.log(convertedDate);
```
Output: 16/03/2021

Du kan också ange vilket land du vill ha formatet för genom att byta ut `'sv-SE'` mot koden för det specifika landet. Till exempel så skulle `'en-US'` ge formatet mm/dd/yyyy.

## Deep Dive:
Uppgiften att konvertera ett datum till en sträng är en nödvändig del av att hantera och visa datum i ett program. Det finns också alternativ till att använda `toLocaleDateString()` såsom att manuellt formatera datumet eller använda ett externt bibliotek, beroende på dina specifika behov.

`toLocaleDateString()` använder sig av `Intl` objektet i JavaScript som implementerades för att möjliggöra språkospecifik formattering av olika typer av data. Det stödjer flera olika format, inklusive klockslag och valuta. 

## Se också:
- [MDN Web Docs - toLocaleDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [MDN Web Docs - Intl](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl)
- [Moment.js](https://momentjs.com/) - ett populärt bibliotek för att hantera, manipulera och visa datum och tider i JavaScript.