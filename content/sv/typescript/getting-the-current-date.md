---
title:                "Att få den nuvarande datumet"
html_title:           "TypeScript: Att få den nuvarande datumet"
simple_title:         "Att få den nuvarande datumet"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datum och tidsinformationen kan vara avgörande för många olika typer av applikationer och program. Det kan användas för att visa användarna när en händelse inträffade, för att räkna tiden mellan två händelser eller för att planera framtida händelser.

Att använda TypeScript för att hämta den aktuella datumet ger dig enkelheten och flexibiliteten som behövs för att hantera datum och tider i dina projekt.

## Hur man gör

För att hämta det aktuella datumet i TypeScript kan du använda JavaScript-objektet `Date`. Detta objekt ger dig möjlighet att hämta inte bara datumet utan även tiden, timmar, minuter, sekunder och mycket mer.

Det första steget är att skapa en ny instans av `Date`-objektet. Detta kan göras genom att använda `new`-nyckelordet tillsammans med `Date`-konstruktorn. Till exempel:

```TypeScript
let currentDate = new Date();
```

Nu när `currentDate`-variabeln har skapats kan du enkelt hämta olika delar av datumet genom att använda `get`-metoder på objektet. Till exempel:

```TypeScript
let year = currentDate.getFullYear();
let month = currentDate.getMonth();
let day = currentDate.getDate();
```

Notera att månaden hämtas som ett tal, där januari motsvarar 0 och december 11. Om du vill ha månadens namn istället kan du använda en array för att översätta det:

```TypeScript
let months = ['januari', 'februari', 'mars', 'april', 'maj', 'juni', 'juli', 'augusti', 'september', 'oktober', 'november', 'december'];
let monthName = months[currentDate.getMonth()];
```

Du kan också hämta tiden eller andra tidsrelaterade värden genom att använda `get`-metoder på samma sätt. Till exempel:

```TypeScript
let hours = currentDate.getHours();
let minutes = currentDate.getMinutes();
let seconds = currentDate.getSeconds();
```

## Djupdykning

Ett viktigt koncept att förstå när du arbetar med datum och tider i TypeScript är att de är baserade på UTC (Coordinated Universal Time). Detta betyder att du måste ta hänsyn till tidszoner när du hanterar datum och tider.

Ett annat vanligt problem är att jämföra två datum. Det är viktigt att förstå att när du skapar en ny instans av `Date`-objektet, skapar du inte bara ett datum utan också en tid. Detta innebär att om du försöker jämföra två datum med `==`-operatören, kommer det att jämföra tiden också. Om du bara vill jämföra datumen kan du använda `date.toDateString()` för att konvertera det till en sträng utan tiden.

Det finns också tredjepartsbibliotek som Moment.js som kan hjälpa till att göra hanteringen av datum och tider enklare och mer robust i TypeScript-projekt.

## Se även

- [Dokumentation för `Date`-objektet på MDN](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js dokumentation](https://momentjs.com/docs/)
- [TypeScript dokumentationen för datum och tider](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)