---
title:                "Beräkning av en framtida eller förfluten datum"
html_title:           "Javascript: Beräkning av en framtida eller förfluten datum"
simple_title:         "Beräkning av en framtida eller förfluten datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller förflutna är en användbar färdighet för programmerare inom många olika områden, som till exempel att skapa tidsscheman för inloggningssystem eller att beräkna förseningar i leveranser. Det är också ett vanligt förekommande problem inom programmering och är därför bra att ha kunskap om.

## Så här gör du

För att beräkna ett datum i framtiden eller förflutna i Javascript använder vi oss av Date-objektet och dess inbyggda metoder. Här är ett enkelt exempel där vi beräknar ett datum en vecka framåt:

```Javascript
var today = new Date(); // skapar ett Date-objekt med dagens datum
var nextWeek = new Date(today.getTime() + 7 * 24 * 60 * 60 * 1000); // adderar 7 dagar till dagens datum
console.log("Datum om en vecka: " + nextWeek);
```

Output: Datum om en vecka: Mon May 10 2021 14:50:25 GMT+0200 (Central European Summer Time)

För att beräkna ett datum i förflutna, subtraherar vi istället antalet millisekunder från det önskade datumet. Här är ett exempel där vi beräknar ett datum för en månad sedan:

```Javascript
var today = new Date(); // skapar ett Date-objekt med dagens datum
var lastMonth = new Date(today.getTime() - 30 * 24 * 60 * 60 * 1000); // subtraherar 30 dagar från dagens datum
console.log("Datum för en månad sedan: " + lastMonth);
```

Output: Datum för en månad sedan: Sun Apr 11 2021 14:55:48 GMT+0200 (Central European Summer Time)

Det är också möjligt att beräkna datum utifrån ett specifikt datum istället för dagens. I följande exempel beräknar vi ett datum 5 dagar efter den 1 maj 2021:

```Javascript
var specificDate = new Date(2021, 4, 1); // skapar ett Date-objekt för den 1 maj 2021
var resultDate = new Date(specificDate.getTime() + 5 * 24 * 60 * 60 * 1000); // adderar 5 dagar
console.log("Datum 5 dagar efter den 1 maj 2021: " + resultDate);
```

Output: Datum 5 dagar efter den 1 maj 2021: Sun May 06 2021 00:00:00 GMT+0200 (Central European Summer Time)

## Djupdykning

När vi använder oss av Date-objektet för att beräkna datum i framtiden eller förflutna är det viktigt att komma ihåg att det inte tar hänsyn till skottår och sommartid. Därför kan det vara bra att använda sig av ett externt bibliotek för att få mer exakta beräkningar.

Ett annat sätt att beräkna datum i framtiden eller förflutna är att använda sig av operators som "+" och "-". I dessa operators har man möjlighet att ange dagar, månader, år osv. som man vill addera eller subtrahera från ett datum.

## Se även

- [MDN:s dokumentation om Date-objektet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date-fns biblioteket för mer exakta datumberäkningar](https://date-fns.org/)