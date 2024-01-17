---
title:                "Jämföra två datum"
html_title:           "Javascript: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum är en vanlig uppgift för programmerare när de arbetar med tidsbaserade funktioner. Genom att jämföra två datum kan vi kontrollera om de är lika, om ett datum är senare än det andra eller om de befinner sig inom ett visst tidsintervall.

## Hur man gör:
```Javascript
// Skapa två datum-objekt
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-02-01');

// Jämför om date1 är lika med date2
if(date1 === date2){
    console.log("Date 1 är lika med Date 2");
} else{
    console.log("Date 1 är inte lika med Date 2");
}

// Jämför om date1 är senare än date2
if(date1 > date2){
    console.log("Date 1 är senare än Date 2");
} else{
    console.log("Date 1 är inte senare än Date 2");
}

//Jämför om date1 är inom ett visst tidsintervall (här 30 dagar) från date2
if(Math.abs(date1 - date2) <= 1000*60*60*24*30){
    console.log("Date 1 är inom ett tidsintervall (30 dagar) från Date 2");
} else{
    console.log("Date 1 är inte inom ett tidsintervall (30 dagar) från Date 2");
}
```

Output: 
```
Date 1 är inte lika med Date 2
Date 1 är inte senare än Date 2
Date 1 är inom ett tidsintervall (30 dagar) från Date 2
```

## Djupdykning:
Att kunna jämföra datum är en viktig del av att kunna hantera tidsbaserade funktioner inom programmering. Historiskt sett har det funnits olika sätt att representera datum inom datorer och ibland kan det finnas skillnader i hur olika programmeringsspråk hanterar datum. Det är därför viktigt att ha en god förståelse för hur just det aktuella programmeringsspråket jämför datum.

En alternativ metod för att jämföra datum är att använda sig av timestamp- eller epoch-formatet, där ett datum representeras som antalet sekunder eller millisekunder som har passerat sedan ett specifikt datum och tidpunkt. Dessa format är vanligtvis mer exakta och pålitliga vid jämförelse av datum.

## Se även:
- [MDN - Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date Comparison](https://www.w3schools.com/js/js_date_methods_compare.asp)