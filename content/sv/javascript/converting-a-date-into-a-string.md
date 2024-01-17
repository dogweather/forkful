---
title:                "Omvandla ett datum till en sträng"
html_title:           "Javascript: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När vi pratar om att konvertera ett datum till en sträng i Javascript, så syftar vi på att ändra formatet på datumet från ett numeriskt värde till en läsbar textsträng. Detta görs oftast när vi vill presentera datum på ett mer lättförståeligt sätt för användaren. 

Programmerare använder denna funktion för att visa datum på ett sätt som gör det enkelt för användaren att läsa och förstå. Dessutom kan det användas för att sortera datum i en viss ordning eller för att jämföra datum i ett program.

## Så här gör du:

För att konvertera ett datum till en sträng i Javascript kan vi använda inbyggda funktioner som `toISOString()` eller `toLocaleString()`. Här är ett exempel på hur man använder `toLocaleString()` för att konvertera ett datum till en textsträng:

```Javascript
let today = new Date(); // skapar ett nytt datumobjekt
let stringDate = today.toLocaleString(); // konverterar datumet till en sträng
console.log(stringDate); // output: "YYYY-MM-DD HH:mm:ss" 
```

Om du önskar ett annat format på din datumsträng, kan du specificera önskad formatering med hjälp av en `options` parameter. Nedan är ett exempel på en konvertering med en specifik formatmall:

```Javascript
let today = new Date();
let options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }; // specifierar formatmall
let stringDate = today.toLocaleString('sv-SE', options); // konverterar datumet till en sträng med formatmallen
console.log(stringDate); // output: "Tisdag, 2 februari 2021"
```

## Djupdykning:

Historiskt sett har det funnits olika sätt att representera och presentera datum i olika länder och kulturer. Detta har lett till utvecklingen av standarder som ISO 8601 som används för att förenkla kommunikationen av datum över gränser. Därför är `toISOString()` enligt denna standard och ger alltid ett datum i formatet "YYYY-MM-DDTHH:mm:ss.sssZ". 

I vissa fall kan det vara lättare att hantera datum som numeriska värden, som antalet millisekunder som förflutit sedan Epoch (1 januari 1970). Då kan man använda funktionen `new Date()` för att konvertera dessa värden till ett datumobjekt och sedan använda sig av ovanstående metoder för att omvandla det till en sträng.

Om du behöver mer flexibilitet i formateringen av datumsträngen kan du också använda dig av tredjepartsbibliotek som Moment.js eller Day.js som ger fler alternativ och möjligheter att anpassa formateringen efter dina behov.

## Se även:

- [JS Date Object - W3Schools](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Date - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 Date - Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)