---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Arbeta med dagens datum i JavaScript

## Vad och Varför?

Att hämta dagens datum i JavaScript innebär att du får en dynamisk tidsstämpel som representerar det nuvarande ögonblicket. Det är värdefullt för att spåra händelser, logga data och skapa funktioner baserade på tid.

## Hur man gör:

Här följer en kodsnutt som visar hur du hämtar dagens datum i JavaScript:

```Javascript
let idag = new Date();
console.log(idag);
```

Detta ger en utmatning som ser ut något så här:

```Javascript
2022-03-04T08:00:00.000Z
```

Vi kan också bryta ner datumobjektet till specifika delar:

```Javascript
let idag = new Date();
console.log(`År: ${idag.getFullYear()} Månad: ${idag.getMonth()} Dag: ${idag.getDate()}`);
```

Detta skapar följande utmatning:

```Javascript
År: 2022 Månad: 3 Dag: 4
```

## Djupdykning

Hämtning av dagens datum i JavaScript är en rutin sak, men det har en rik historisk kontext. JavaScripts Date-objekt introducerades i den första utgåvan av ECMAScript (ES1) 1997.

Det finns alternativ att ta hänsyn till. Om du arbetar med komplexa datum och tidshantering, kan bibliotek som Moment.js eller Day.js vara användbara. Dessa bibliotek erbjuder mer robusta verktyg för att hantera tidszoner, datumformat och beräkningar.

JavaScripts datumobjekt fungerar genom att lagra antalet millisekunder sedan Unixepoken (01 januari 1970, vid midnatt UTC). När du skapar ett nytt datumobjekt utan argument (`new Date()`) returneras antalet millisekunder från Unixepoken till nuvarande tidpunkt.

## Se också

Om du vill läsa mer om datum och tid i JavaScript, kolla in följande källor:

- [MDN dokumentation om Date objektet](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript.info guide till datum och tid](https://javascript.info/date)
- [Moment.js officiella webbplats](https://momentjs.com/)
- [Day.js officiella webbsida](https://day.js.org/)