---
title:    "Javascript: Beräkna ett datum i framtiden eller förflutna"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför 

Beräkning av ett datum i framtiden eller det förflutna kan vara en användbar programmeringsfärdighet för att automatisera processer eller skapa dynamiska tidsbaserade funktioner. Det kan också vara en rolig utmaning för programmerare att utveckla algoritmer för att göra detta.

## Så här gör du

För att beräkna ett datum i framtiden eller förflutna i Javascript, kan man använda Date-objektet och dess metoder. För att få ett datum 10 dagar framåt, kan man använda följande kod:

```Javascript 
let dagensDatum = new Date(); //skapar ett Date-objekt för dagens datum 
let nyttDatum = new Date(dagensDatum.setDate(dagensDatum.getDate() + 10)); //lägger till 10 dagar till dagens datum 
console.log(nyttDatum); //utskrift: 10 dagar framåt i tiden från dagens datum 
```

På samma sätt kan man använda Date-objektet för att få ett datum 10 dagar bakåt. Skillnaden är att man använder sig av `setDate()` metoden med ett negativt värde:

```Javascript 
let dagensDatum = new Date(); 
let nyttDatum = new Date(dagensDatum.setDate(dagensDatum.getDate() - 10)); 
console.log(nyttDatum); //utskrift: 10 dagar bakåt i tiden från dagens datum 
```

Man kan också specificera ett visst datum istället för dagens datum. Till exempel, om man vill ha ett datum 5 månader framåt i tiden från 1 januari 2021, kan man använda följande kod:

```Javascript 
let startDatum = new Date("2021-01-01"); //skapar ett Date-objekt för 1 januari 2021 
let nyttDatum = new Date(startDatum.setMonth(startDatum.getMonth() + 5)); //lägger till 5 månader till startdatumet 
console.log(nyttDatum); //utskrift: 1 juni 2021 
```

## Djupdykning 

Hela Date-objektet och dess metoder kan vara ganska komplexa att förstå och det finns många olika sätt att manipulera datum och tid i Javascript. Det är viktigt att förstå skillnaderna mellan olika metoder som `setDate()` och `setFullYear()` för att kunna skapa korrekta beräkningar. Att hantera tidszoner kan också vara en utmaning att hantera vid datumberäkningar.

Referera till Javascripts dokumentation för mer detaljerad information om Date-objektet och dess metoder.

## Se även

- [Datum och tid i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Datetime bibliotek för Javascript](https://momentjs.com/)
- [Tidszonskonvertering i Javascript](https://momentjs.com/timezone/)