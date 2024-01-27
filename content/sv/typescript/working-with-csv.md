---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Jobbar med CSV för att hantera data i textformat, separerade med kommatecken. Programmörer gör det för att enkelt utbyta data mellan olika system.

## How to:
```TypeScript
import * as fs from 'fs';
import * as parse from 'csv-parse/lib/sync';

// En funktion för att läsa och parse:a en CSV-fil
function readCSV(filePath: string): any[] {
    const fileContent = fs.readFileSync(filePath);
    const records = parse(fileContent, {
        columns: true,
        skip_empty_lines: true,
    });
    return records;
}

// Använd funktionen för att läsa en CSV-fil
const csvData = readCSV('data.csv');
console.log(csvData);
```
Output skulle se ungefär ut såhär om `data.csv` innehåller:
```
Name,Age,Occupation
Alice,30,Engineer
Bob,22,Designer
```
```JSON
[
  { "Name": "Alice", "Age": "30", "Occupation": "Engineer" },
  { "Name": "Bob", "Age": "22", "Occupation": "Designer" }
]
```

## Deep Dive
CSV, eller "Comma-separated values", uppfanns på 1970-talet. Alternativ inkluderar JSON och XML, men CSV är enklast. Implementationsdetaljer kräver att man hanterar varje rad som en datarad, skiljer på kolumner med kommatecken och tänker på inkonsekventa radbrytningar.

## See Also
- Node.js `fs` modul Docs: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- `csv-parse` lib: [https://csv.js.org/parse/](https://csv.js.org/parse/)
- RFC 4180 om CSV standard: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- TypeScript grundläggande: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
