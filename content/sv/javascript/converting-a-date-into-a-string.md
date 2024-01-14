---
title:                "Javascript: Konvertera ett datum till en sträng"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför: 

I Javascript är det vanligt att man behöver konvertera datum till en sträng när man arbetar med datumobjekt. Detta kan vara för att presentera datum på ett mer läsbart sätt för användaren, eller för att jämföra datum i en databas. I denna artikel kommer jag att gå igenom hur man konverterar ett datum till en sträng på ett enkelt sätt.

## Hur man gör:

För att konvertera ett datum till en sträng i Javascript finns det en inbyggd funktion som heter `toString()`. Detta gör att vi kan konvertera ett datumobjekt till en sträng på ett snabbt och enkelt sätt. Här är ett exempel som visar hur man använder `toString()`:

```Javascript
var today = new Date(); // skapar ett datumobjekt för dagens datum
var dateString = today.toString(); // konverterar datumet till en sträng
console.log(dateString); // output: Tue Aug 17 2021 09:44:27 GMT+0200 (Central European Summer Time)
```

I det här fallet har vi skapat ett nytt datumobjekt för dagens datum och sedan använt `toString()` för att konvertera det till en sträng. Nu kan vi använda den nya strängen för att presentera datumet på ett mer läsbart sätt eller för att jämföra det med andra datum.

## Djupdykning:

Förutom `toString()` finns det också andra sätt att konvertera datum till strängar i Javascript. Ett exempel är `toLocaleDateString()`, som gör att datumet visas i ett format som är anpassat efter det lokala språket och regionen. Här är ett exempel på hur man kan använda det:

```Javascript
var date = new Date(2021, 7, 17); // skapar ett datumobjekt för 17 augusti 2021
var options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }; // ändrar formatteringsalternativ
var dateString = date.toLocaleDateString("sv-SE", options); // konverterar datumet till en sträng med svenska formatteringsalternativ
console.log(dateString); // output: tisdag den 17 augusti 2021
```

Genom att använda olika formatteringsalternativ kan man anpassa utseendet på den genererade strängen för att bättre passa ens syften.

## Se även:

Här är några användbara resurser för att lära sig mer om hur man hanterar datum och tid i Javascript:

- [MDN webbdokumentation: Date](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Javascript Date](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [JavaScript date libraries comparison](https://date-fns.org/)

Lycka till med att konvertera datum till strängar i dina Javascript-program!