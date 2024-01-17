---
title:                "Hämta aktuellt datum"
html_title:           "Javascript: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att hämta det aktuella datumet i Javascript är ett vanligt programmeringsproblem. Det är användbart när man behöver visa datumet på en webbsida eller när man vill beräkna tiden mellan två datum.

# Hur gör man?

Det finns flera sätt att hämta det aktuella datumet i Javascript. Ett enkelt sätt är att använda Date-objektet och dess inbyggda metoder. Här är ett exempel på hur man kan göra det:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

Detta kommer att skriva ut det aktuella datumet och tiden i konsolen, som t.ex. "Tue Jun 22 2021 11:31:24 GMT+0200 (Central European Summer Time)".

Om man bara vill hämta datumet utan tiden kan man använda metoden ```getDate()```:

```Javascript
let currentDate = new Date();
let date = currentDate.getDate();
console.log(date);
```

Detta kommer att skriva ut det aktuella datumet, som t.ex. "22".

# Fördjupning

Att skapa och manipulera datum i datorprogram är ett vanligt problem som har funnits sedan de första programmeringsspråken utvecklades. Tidigare fanns inte inbyggda funktioner för detta och man var tvungen att skriva mer komplicerad kod för att få ut det aktuella datumet.

Numera finns det också andra sätt att hämta det aktuella datumet i Javascript, som t.ex. att använda bibliotek som Moment.js eller Day.js. Dessa bibliotek erbjuder fler möjligheter och är extra användbara om man behöver bearbeta eller formatera datumet på olika sätt.

För att implementera detta i din egna kod behöver du först skapa ett Date-objekt och sedan använda dess inbyggda metoder för att hämta önskad information. Det är också viktigt att vara medveten om skillnader i datumformat mellan olika länder och att göra nödvändiga justeringar.

# Se även

- [Mozilla Developer Network - Date](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Day.js](https://day.js.org/)