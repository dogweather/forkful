---
title:    "Javascript: Jämföring av två datum"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering, speciellt när det kommer till hantering av tidsbaserade data eller funktioner. Genom att förstå hur man kan jämföra två datum i Javascript kan du effektivt hantera och manipulera dina tidsdata.

## Så här gör du

För att jämföra två datum i Javascript, använd följande syntax:

```Javascript
var datum1 = new Date("2021-01-01");
var datum2 = new Date("2021-01-05");

// Jämför datum1 med datum2
if (datum1 > datum2) {
    console.log("Datum 1 är senare än datum 2");
} else if (datum1 < datum2) {
    console.log("Datum 2 är senare än datum 1");
} else {
    console.log("Datumen är lika");
}
```

Resultatet av detta kodblock beror på de specifika datumen som används. Om datum1 är senare än datum2 kommer "Datum 1 är senare än datum 2" att skrivas ut i konsolen. Om datum2 är senare kommer "Datum 2 är senare än datum 1" att skrivas ut. Om de båda datumen är exakt samma kommer "Datumen är lika" att skrivas ut.

För att jämföra datum baserat på en specifik tidsenhet, som till exempel enbart år eller månad, kan du använda de inbyggda metoder som finns tillgängliga för Date-objektet. Här är ett exempel på hur du jämför två datum baserat på enbart år:

```Javascript
// Skapa två datum som bara tar hänsyn till år
var datum1 = new Date("2020-01-01");
var datum2 = new Date("2021-01-01");

// Jämför år med hjälp av inbyggda metoder
if (datum1.getFullYear() > datum2.getFullYear()) {
    console.log("Datum 1 är senare än datum 2");
} else if (datum1.getFullYear() < datum2.getFullYear()) {
    console.log("Datum 2 är senare än datum 1");
} else {
    console.log("Datumen är lika");
}
```

I detta fall kommer "Datum 2 är senare än datum 1" att skrivas ut eftersom datum2 är ett år senare än datum1.

## Djupdykning

Vid jämförelse av två datum är det viktigt att förstå hur Javascript hanterar tidszoner och sommartider. Datum-objektet i Javascript lagrar datumet som ett värde i millisekunder från 1 januari 1970 00:00:00 UTC, vilket gör det enkelt att jämföra datum oavsett tidszon. Men när datumet ska visas ut till användaren, justeras det för lokala tidszoner, vilket kan påverka resultatet av jämförelser.

Sommartid kan också påverka jämförelser av datum då en timme kan adderas eller subtraheras beroende på tidszon och datumets position inom sommartiden.

För att undvika dessa problem är det viktigt att noga hantera tidszoner och sommartid när du jämför datum i Javascript.

## Se även

- [Mozilla Developer Network - Date](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Javascript Date Objects](https://www.w3schools.com/js/js_dates.asp)
- [Date.prototype.getFullYear()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getFullYear)