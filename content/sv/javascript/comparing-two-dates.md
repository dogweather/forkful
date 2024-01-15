---
title:                "Jämförande av två datum"
html_title:           "Javascript: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att kunna jämföra två datum är en viktig del av programmering. Det låter oss utföra olika handlingar beroende på hur datumet förhåller sig till varandra, vilket kan vara användbart i olika typer av applikationer.

## Hur man jämför två datum i JavaScript

För att kunna jämföra två datum i JavaScript finns det några olika metoder som kan användas.

1. Använda <, >, <= eller >= operatörerna för att jämföra två datum. Till exempel:

```javascript
const date1 = new Date(2021, 0, 1);
const date2 = new Date(2021, 5, 1);

if (date1 < date2) {
  console.log("Datum 1 är tidigare än datum 2");
}
```

Output: `Datum 1 är tidigare än datum 2`

2. Använda `getTime()` metoden för att få datumets millisekund-representation och sedan jämföra dessa. Till exempel:

```javascript
const date1 = new Date(2021, 0, 1);
const date2 = new Date(2021, 5, 1);

if (date1.getTime() < date2.getTime()) {
  console.log("Datum 1 är tidigare än datum 2");
}
```

Output: `Datum 1 är tidigare än datum 2`

3. Använda `valueOf()` metoden som returnerar datumets millisekund-representation i numerisk form. Till exempel:

```javascript
const date1 = new Date(2021, 0, 1);
const date2 = new Date(2021, 5, 1);

if (date1.valueOf() < date2.valueOf()) {
  console.log("Datum 1 är tidigare än datum 2");
}
```

Output: `Datum 1 är tidigare än datum 2`

## Djupdykning

Det finns en viktig sak att komma ihåg när man jämför två datum i JavaScript: de jämförs baserat på datumets millisekund-representation. Detta innebär att två olika datum kan ha samma millisekund-värde och därmed anses vara lika, vilket kan leda till oväntade resultat.

För att undvika detta problem kan man använda metoder som `getFullYear()`, `getMonth()` eller `getDate()` för att jämföra specifika delar av datumet istället för hela datumet.

## Se även

- [MDN web docs: Date](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: JavaScript Date Comparison](https://www.w3schools.com/js/js_date_comparisons.asp)
- [Stack Overflow: How to compare dates in JavaScript](https://stackoverflow.com/questions/49299418/how-to-compare-dates-in-javascript)