---
title:                "Arbeta med csv"
html_title:           "Javascript: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Working with CSV står för Comma Separated Values och är ett vanligt filformat för att lagra och utbyta tabellformatad data. Det är användbart för programmerare eftersom det gör det möjligt att enkelt importera och exportera data mellan olika program och system.

## Så här gör man: 
För att arbeta med CSV i Javascript, kan man använda sig av ett externt bibliotek som heter "fast-csv". Detta bibliotek gör det enkelt att läsa och skriva CSV-filer i Javascript. Ett exempel på hur man kan läsa och logga en CSV-fil med hjälp av "fast-csv" ser ut såhär:
```Javascript
const csv = require('fast-csv');

csv.parseFile('example.csv', { headers: true })
    .on('data', data => {
        console.log(data);
    })
    .on('error', error => {
        console.log(error);
    });
```

Detta kodexempel läser in en CSV-fil som heter "example.csv" och loggar varje rad i konsolen som ett objekt med hjälp av filens kolumnrubriker som egenskaper.

För att skriva en CSV-fil med hjälp av "fast-csv" kan man använda sig av följande kod:
```Javascript
const csv = require('fast-csv');

csv.writeToPath('example.csv', [
    ["Name", "Age"],
    ["John", 30],
    ["Sarah", 25]
]);
```
Detta kodexempel skapar en CSV-fil med namnet "example.csv" och lägger till två rader med data: Namn och ålder för John och Sarah.

## Djupdykning:
CSV-filer har funnits sedan 1972 och har sedan dess blivit ett standardformat för att utbyta tabellformatad data. Det är ett enkelt och lättläst format som kan läsas och användas av de flesta program och språk. Dessutom finns det många alternativ till "fast-csv" för att arbeta med CSV-filer i Javascript, såsom "csv-parse" och "csv-writer".

Det finns även möjlighet att implementera en egen funktion för att läsa och skriva CSV-filer utan att använda sig av ett externt bibliotek. Det kan vara en bra övning för att förbättra sina programmeringskunskaper och förstå hur CSV-filer fungerar.

## Se också:
- [CSV-writer](https://www.npmjs.com/package/csv-writer)
- [csv-parse](https://www.npmjs.com/package/csv-parse)
- [Wikipedia: CSV](https://sv.wikipedia.org/wiki/CSV)