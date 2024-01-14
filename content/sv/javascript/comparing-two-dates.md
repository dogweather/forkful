---
title:    "Javascript: Jämförelse av två datum"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en bra programmeringsfärdighet att ha, eftersom det kan hjälpa dig att avgöra om ett datum är tidigare, senare eller samma som ett annat datum. Detta är särskilt användbart när du arbetar med tidsberoende data eller vill göra datumbaserade beräkningar.

## Hur man gör

För att jämföra två datum i Javascript kan du använda metoden `getTime()` som returnerar antalet millisekunder som förflutit sedan 1 januari 1970. Genom att använda denna metod kan du få en numerisk representation av varje datum och sedan jämföra dem.

```Javascript
let date1 = new Date("2021-10-10");
let date2 = new Date("2021-10-12");

// Jämför date1 och date2
if (date1.getTime() > date2.getTime()) {
  console.log("date1 är senare än date2");
} else if (date1.getTime() < date2.getTime()) {
  console.log("date1 är tidigare än date2");
} else {
  console.log("date1 och date2 är samma datum");
}

// Output: date1 är tidigare än date2
```

Om du vill jämföra datum baserat på år, månad eller dag, kan du använda andra metoder som `getFullYear()`, `getMonth()` och `getDate()` på varje datum och sedan jämföra resultaten.

```Javascript
let date1 = new Date("2021-10-10");
let date2 = new Date("2021-10-12");

// Jämför år på date1 och date2
if (date1.getFullYear() > date2.getFullYear()) {
  console.log("date1 är senare år än date2");
} else if (date1.getFullYear() < date2.getFullYear()) {
  console.log("date1 är tidigare år än date2");
} else {
  console.log("date1 och date2 är samma år");
}

// Output: date1 och date2 är samma år
```

## På djupet

Att jämföra datum kan vara lite knepigt eftersom det finns många faktorer att ta hänsyn till, som till exempel årsskiften och skottår. Det är viktigt att förstå hur `getTime()`-metoden fungerar och att vara konsekvent med vilket datumformat du använder för att få exakta resultat.

Ett annat tips är att konvertera datumen till samma tidszon för att undvika felaktiga jämförelser på grund av tidszoners skillnader.

## Se även

- How to Work with Dates in JavaScript: https://www.w3schools.com/js/js_dates.asp
- Date.prototype.getTime(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime
- Date.prototype.getFullYear(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getFullYear