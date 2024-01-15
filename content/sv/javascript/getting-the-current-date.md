---
title:                "Att få den aktuella datumet"
html_title:           "Javascript: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att veta den nuvarande tiden och datumet kan vara väldigt användbart för att organisera ens liv, hålla deadlines och spåra aktiviteter. Med hjälp av Javascript kan du enkelt hämta den aktuella datumen och tiden direkt från din webbläsare.

## Så här gör du

Det finns flera olika sätt att få den aktuella datumen och tiden i Javascript, beroende på vilken precision du behöver och vilken version av Javascript du använder. Här är några enkla exempel för att komma igång:

```Javascript
// Hämtar hela datumen (år, månad och dag)
const datum = new Date(); 
console.log(datum); // Utmatning: Sun Feb 14 2021 00:00:00 GMT+0100 (Central European Standard Time)

// Hämtar ett specifikt år
const år = datum.getFullYear(); 
console.log(år); // Utmatning: 2021

// Hämtar en specifik månad (index börjar alltid på 0, så 0 är januari och 11 är december)
const månad = datum.getMonth(); 
console.log(månad); // Utmatning: 1 (februari)

// Hämtar en specifik dag i månaden
const dag = datum.getDate(); 
console.log(dag); // Utmatning: 14 (idag)

// Hämtar den nuvarande tiden (timmar, minuter och sekunder)
const tid = datum.toLocaleTimeString(); 
console.log(tid); // Utmatning: 00:00:00 (midnatt)
```

Som du kan se är det ganska enkelt att få den aktuella datumen och tiden med hjälp av Javascript. Det finns också andra metoder som du kan använda beroende på dina behov, som att hämta den lokala tiden, den universella tiden eller ens lägga till en viss tid till det aktuella datumet.

## Deep Dive

För de som är intresserade av mer komplexa funktioner för att hämta och manipulera datumen och tiden, finns det ett objekt som heter "Date" i Javascript. Detta objekt har olika metoder och egenskaper som kan användas för att få detaljerade uppgifter om datum och tid.

Till exempel, om du vill hämta det exakta klockslaget, inte bara timmar, minuter och sekunder, kan du använda metoden "getHours()" tillsammans med "getMinutes()" och "getSeconds()" för att få exakta värden. Du kan också använda "setDate()" metoden för att ändra ett specifikt datum i månaden eller "setHours()" för att ställa in en specifik tid på dagen.

Du kan utforska alla de tillgängliga metoderna och egenskaperna för "Date" objektet i Javascript dokumentationen.

## Se också

- [Get Current Date and Time in JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [The Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Manipulating Date and Time in JavaScript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)