---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Konvertera ett datum till en strängmed Javascript: Din enkla guide 

## Vad & Varför?
Att konvertera ett datum till en sträng innebär att byta format från dateobject-form till oläslig textform. Varför göra det? Det gör datum lättare att visa och manipulera i dina webbapplikationer.

## Hur göra:
Här är några exempel på hur du gör om ett datum till en sträng i Javascript:

```javascript
// Skapa nytt datum
let datum = new Date();

// Konvertera till sträng
let datumSomStrang = datum.toString();

console.log(datumSomStrang);
```
Ett exempel på utdata kan vara något sådant här:
```
Sat Apr 17 2021 20:29:12 GMT+0200 (Central European Summer Time)
```

Eller använda `toISOString` för att få datumet i ISO-format:

```javascript
// Skapa nytt datum
let datum = new Date();

// Konvertera till sträng i ISO-format
let datumSomISO = datum.toISOString();

console.log(datumSomISO);
```
Utdatan blir ISO formaterad tid och kan se ut så här:
```
2021-04-17T18:29:12.855Z
```

## Djupdykning
Konvertering av datum till sträng hade milda historiska utmaningar, särskilt med tidzoner. Gamla sätt att hantera detta inkluderade användning av tredjepartsbibliotek som Moment.js. Alternativa metoder för att göra detta idag inkluderar metoden `toLocaleDateString()`, vilket ger en läsbar sträng som respekterar det lokala språket och formatet. Rörande implementering, inbyggda Javascript-metoder som `toString()`, `toISOString()`, och `toLocaleDateString()` manipulerar hur datumet presenteras utan att ändra det ursprungliga datumobjektet.

## Se även
För vidare läsning, se dessa användbara resurser:

1. [JavaScript Date Referens](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [JavaScript Date toString() Metod](https://www.w3schools.com/jsref/jsref_tostring_date.asp)
3. [JavaScript ISO Datum](https://www.w3schools.com/js/js_date_formats.asp)