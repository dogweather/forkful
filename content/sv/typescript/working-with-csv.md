---
title:                "Arbeta med csv"
html_title:           "TypeScript: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV är en vanlig uppgift för programmerare. CSV står för "Comma Separated Values" och är ett filformat som används för att lagra tabellinformation. Det är vanligt att använda CSV för att importera eller exportera data från databaser eller arbetsblad.

## Så här gör du:

Här är ett enkelt exempel på hur du kan arbeta med CSV i TypeScript:

```TypeScript
// Importera paketet för att arbeta med CSV
import * as csv from 'csv-parser';
// Här importerar vi även "fs" - modulen för att läsa och skriva filer
import * as fs from 'fs';

// Skapa en ny tom array för att lagra vår data
let data = [];
// Använd "fs" för att läsa in vår CSV-fil
fs.createReadStream('minfil.csv')
  .pipe(csv()) // Konvertera filen till ett objekt som vi kan arbeta med
  .on('data', (row) => { // Loopa igenom varje rad i filen
    data.push(row); // Lägg till raden i vår array
  })
  .on('end', () => { // När läsningen är klar
    console.log(data); // Skriv ut vår datarray
  });
```

Exempel på utskrift av vår datarray:

```
[
  { namn: 'Anna', ålder: '26', yrke: 'programmerare' },
  { namn: 'Erik', ålder: '32', yrke: 'designer' },
  { namn: 'Sara', ålder: '24', yrke: 'marknadsförare' }
]
```

## Djupdykning

CSV-formatet utvecklades på 1970-talet som ett sätt att lagra data i tabellform. Det är ett vanligt format för att importera och exportera data från olika program och plattformar. Alternativ till CSV inkluderar JSON och XML, men CSV är fortfarande ett populärt val för enkla datatyper.

Implementationen av att arbeta med CSV i TypeScript är enkel tack vare paketet "csv-parser" som gör det möjligt att läsa och konvertera CSV-filer till användbara objekt för programmering.

## Se även

- [Csv-parser på npm](https://www.npmjs.com/package/csv-parser)
- [Wikipedia-artikel om CSV](https://sv.wikipedia.org/wiki/CSV)