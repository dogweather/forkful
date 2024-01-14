---
title:    "Javascript: Att få den nuvarande datumet"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumet är av stor vikt för många Javascript programmerare. Det kan vara användbart för att visa datumet till användare, sortera data efter datum eller för att utföra specifika uppgifter baserat på aktuell dag. Oavsett vad ditt syfte är, är det viktigt att veta hur man hämtar och hanterar datum i Javascript.

## Hur man gör det

För att hämta den aktuella datumet i Javascript kan du använda inbyggda objektet Date. Här är ett exempel på hur du kan skriva kod för att hämta dagens datum:

```Javascript
let nu = new Date();
console.log(nu); // Output: Sat Jul 18 2020 11:30:00 GMT+0200 (Central European Summer Time)
```

För att få en mer läsbar format kan du använda olika inbyggda metoder som `.toDateString()` eller `.toLocaleDateString()`:

```Javascript
let nu = new Date();
console.log(nu.toDateString()); // Output: Sat Jul 18 2020
console.log(nu.toLocaleDateString()); // Output: 2020-07-18
```

Du kan också hämta olika delar av datumet som månad, dag och år genom att använda inbyggda metoder som `.getDate()`, `.getMonth()` och `.getFullYear()`:

```Javascript
let nu = new Date();
console.log(nu.getDate()); // Output: 18
console.log(nu.getMonth() + 1); // Månaden är indexerad från 0 så vi lägger till 1 för att få rätt månad. Output: 7
console.log(nu.getFullYear()); // Output: 2020
```

För att hämta tiden, kan du använda `.getHours()`, `.getMinutes()` och `.getSeconds()`:

```Javascript
let nu = new Date();
console.log(nu.getHours()); // Output: 11
console.log(nu.getMinutes()); // Output: 30
console.log(nu.getSeconds()); // Output: 0
```

## Djupdykning

Det finns många olika sätt att hantera datum i Javascript. En intressant metod som är värd att nämna är `.getTime()`. Detta returnerar antalet millisekunder sedan 1 januari 1970, vilket ofta kallas "Unix-tiden". Detta värde är användbart om du behöver beräkna tiden som gått mellan två datum eller om du vill jämföra datum.

```Javascript
let nu = new Date();
let for_ett_ar_sen = new Date(nu.getFullYear() - 1, nu.getMonth(), nu.getDate()); // Skapar ett datum som är ett år bakåt i tiden
console.log(nu.getTime()); // Output: 1595064600000
console.log(for_ett_ar_sen.getTime()); // Output: 1563423000000
```

Som du kan se är Unix-tiden användbar när man jämför eller beräknar datum.

## Se även

- [W3 Schools: Date Objects in Javascript](https://www.w3schools.com/js/js_date.asp)
- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [FreeCodeCamp: Working with Dates in Javascript](https://www.freecodecamp.org/news/javascript-tutorial-working-with-dates/)