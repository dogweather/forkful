---
title:                "Att arbeta med csv"
html_title:           "TypeScript: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

Det finns många tillfällen då vi behöver hantera stora datamängder i våra mjukvaruprojekt. Att använda Comma Separated Values (CSV) filer ger oss en enkel och effektiv metod för att hantera sådan data. Med hjälp av TypeScript kan vi enkelt läsa, skriva och manipulera CSV filer i våra program.

## Hur man använder TypeScript för att hantera CSV

För att kunna arbeta med CSV filer i TypeScript behöver vi installera och importera ett externt bibliotek som heter "csv-parser". Efter installationen kan vi använda följande kod för att läsa in en CSV fil och spara data i en variabel:

```TypeScript
import * as csv from 'csv-parser'; 

let data = [];
fs.createReadStream('minfil.csv')
    .pipe(csv())
    .on('data', (row) => {
        data.push(row);
    })
    .on('end', () => {
        console.log('Data från CSV filen: ', data);
    });
```

Om vi till exempel har en CSV fil med titlar och författare av böcker, kommer outputen att se ut såhär:

```
Data från CSV filen: 
[
    { title: 'Mästerdetektiven Blomkvist', author: 'Astrid Lindgren' }, 
    { title: 'Fangirl', author: 'Rainbow Rowell' }, 
    { title: 'Harry Potter och den vises sten', author: 'J.K. Rowling' }
]
```

För att skriva data till en CSV fil använder vi en annan metod från biblioteket "csv-writer". Nedan är ett exempel på hur vi kan spara data från en array till en CSV fil:

```TypeScript
import * as fs from 'fs';
import * as csvWriter from 'csv-writer';

let data = [
    { name: 'Johan', age: 35 }, 
    { name: 'Anna', age: 29 }, 
    { name: 'Erik', age: 42 }
];

const writer = csvWriter.createObjectCsvWriter({
    path: 'allaPersoner.csv',
    header: [
        {id: 'name', title: 'Namn'},
        {id: 'age', title: 'Ålder'}
    ]
});

writer.writeRecords(data)
    .then(() => {
        console.log('Data från arrayen sparades i CSV filen');
    });
```

Detta kommer att skapa en CSV fil med namn och ålder för varje person i arrayen.

## Djupdykning

När vi arbetar med CSV filer är det viktigt att förstå hur data lagras och hanteras. CSV filer är baserade på tabeller där varje rad representerar en post och varje kolumn representerar en specifik attribut eller egenskap för denna post. Det är också viktigt att hålla koll på olika separatorer som används i olika CSV filer (kommatecken, punkter, semikolon etc.) och att anpassa vår kod efter detta.

Ett annat sätt att hantera CSV filer är att använda en databas där vi kan importera CSV filen och utföra SQL-frågor på datan. Detta kan vara ett effektivt sätt att hantera stora datamängder och genomföra avancerade analyser.

## Se också

- [csv-parser bibliotek](https://www.npmjs.com/package/csv-parser)
- [csv-writer bibliotek](https://www.npmjs.com/package/csv-writer)
- [Official TypeScript hemsida](https://www.typescriptlang.org/)