---
title:    "Javascript: Omvandla ett datum till en sträng"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att utforska hur man konverterar ett datum till en sträng i Javascript. Att kunna konvertera datum är en viktig färdighet inom programmering och är användbar för att få befintliga datum till mer lättlästa format.

## Så här

Först måste vi förstå hur Javascript hanterar datum. Datum lagras som siffror i millisekunder från Unix epoch-tiden, som är den 1 januari 1970 00:00:00 UTC. För att konvertera detta till en läsbar sträng, måste vi använda inbyggda metoder.

För att konvertera ett datum till en sträng kan vi använda toLocaleDateString() metoden som finns tillgänglig för alla datumobjekt i Javascript. Vi kan också ange olika parametrar för att få det i olika format, till exempel:

```Javascript
const datum = new Date();
console.log(datum.toLocaleDateString("sv-SE")); // 4/3/2020
console.log(datum.toLocaleDateString("sv-SE",{ weekday: 'long' })); // torsdag
console.log(datum.toLocaleDateString("sv-SE",{ year: 'numeric', month: 'long', day: 'numeric' })); // april 3, 2020
```

Som du kan se så kan vi använda olika parametrar för att få önskat datumformat baserat på vårt behov.

## Djupdykning

För att korrekt kunna konvertera ett datum till en sträng, är det viktigt att förstå olika tidszoner och hur de kan påverka resultatet. När ett datum skapas i Javascript, kommer det att representera tidpunkten i den lokala tidszonen på användarens enhet. Detta innebär att konverteringen av datum till sträng kommer att följa samma tidszon.

För att undvika problem med tidszoner, kan vi använda metoden toUTCString() som kommer att konvertera datumet till UTC-tiden och sedan använda toLocaleDateString() metoden för att få det i önskat format enligt den lokala tidszonen.

## Se även

* [Javascript Date Object documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [Understanding Dates and Times in Javascript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)