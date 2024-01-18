---
title:                "Utvinna ett datum från en sträng"
html_title:           "Javascript: Utvinna ett datum från en sträng"
simple_title:         "Utvinna ett datum från en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Vad är datointerpretation och varför behöver programmerare göra det?

Datointerpretation är processen att konvertera en tidsangivelse i en textsträng till en datumobjekt i JavaScript. Detta är användbart när man arbetar med användardata och behöver omvandla text till användbara datumvärden. Det kan också vara en del av datavalidering för att säkerställa att korrekta datumformat används. 

Så här gör du:

```javascript
// Skapa en sträng med ett datum
let dateStr = "12/31/2020";

// Använd Date() konstruktorn för att skapa ett datumobjekt
let dateObj = new Date(dateStr);

// Visa resultatet i konsolen
console.log(dateObj);
```

Resultatet borde visas som: `Thu Dec 31 2020 00:00:00 GMT+0100 (Central European Standard Time)`

## Djupdykning
Historisk kontext:
Datointerpretation har varit en viktig del av programmering sedan början av datortekniken. Innan konceptet av datumobjekt fanns, fick programmerare använda strängmanipulation för att konvertera tidsangivelser till användbara värden.

Alternativ:
Det finns flera bibliotek tillgängliga för datointerpretation i JavaScript, till exempel Moment.js och date-fns. Dessa är användbara om du behöver hantera mer komplexa tidsformat eller behöver fler funktioner för datummanipulering.

Implementation detaljer:
För att konvertera en tidsangivelse i en textsträng till ett datumobjekt, använder JavaScript Datum konstruktorn. Det tar emot argument i formatet "månad/dag/år" och skapar sedan ett nytt Datumobjekt med den angivna tidsangivelsen. Det är viktigt att notera att datumformatet kan variera beroende på region och språkinställningar, så det är viktigt att använda rätt format för dina behov.

## Se även
- [Date() konstruktorn på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)