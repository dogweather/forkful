---
title:                "Javascript: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV (komma avgränsad fil) är en vanlig format för att lagra data i tabeller, särskilt när det gäller stora mängder data eller utbyte mellan olika program. Att kunna arbeta med CSV i Javascript är en viktig färdighet för alla som hanterar datadrivna uppgifter.

## Hur man gör det

För att arbeta med CSV i Javascript, måste du först importera biblioteken "fs" och "csv-parse".

```Javascript
const fs = require('fs');
const csv = require('csv-parse');
```

Nästa steg är att läsa in CSV-filen genom att använda "fs.readFile" och sedan konvertera data till ett objekt med hjälp av "csv.parse" funktionen.

```Javascript
fs.readFile('data.csv', 'utf8', (err, data) => {
  csv(data, {
    columns: true,
    delimiter: ','
  }, (err, output) => {
    console.log(output); // här ser du outputen i konsolen
  });
});
```

I detta exempel använder vi "columns: true" för att omvandla varje rad i CSV-filen till en unik objektens egenskaper. Om du vill spara outputen till en variabel kan du använda "writeFile" funktionen.

```Javascript
fs.readFile('data.csv', 'utf8', (err, data) => {
  csv(data, {
    columns: true,
    delimiter: ','
  }, (err, output) => {
    fs.writeFile('output.json', JSON.stringify(output), (err) => {
      if (err) throw err;
      console.log('Filen sparades!');
    });
  });
});
```

## Djupdykning

Det finns flera saker att tänka på när man arbetar med CSV-filer i Javascript. Till exempel, om din CSV-fil innehåller specialtecken som används som delimitrar, måste du specifiera det när du använder "csv.parse" funktionen.

```Javascript
csv(data, {
  columns: true,
  delimiter: '|' // om ditt CSV-fil använder "|" som delimitrar
}, (err, output) => {
  // kör dina kod här
});
```

Det är också viktigt att notera att med större CSV-filer, är det bäst att använda strömning (streaming) istället för att läsa in hela filen på en gång. Detta minskar minnet som används och förbättrar prestandan.

## Se också

För mer information om att arbeta med CSV i Javascript, ta en titt på dessa resurser:

- [CSV-to-Array](https://www.npmjs.com/package/csv-to-array) - ett enkelt bibliotek som konverterar en CSV-fil till en matris i Javascript.
- [CSV på Wikipedia](https://sv.wikipedia.org/wiki/Comma-separated_values) - en grundläggande översikt över CSV-formatet.