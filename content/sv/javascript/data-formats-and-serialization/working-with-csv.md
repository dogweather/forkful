---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:21.958692-07:00
description: "Att arbeta med CSV (Comma-Separated Values, komma-separerade v\xE4rden)\
  \ i JavaScript inneb\xE4r att tolka eller generera CSV-filer f\xF6r att antingen\
  \ importera\u2026"
lastmod: '2024-03-13T22:44:38.315716-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med CSV (Comma-Separated Values, komma-separerade v\xE4rden)\
  \ i JavaScript inneb\xE4r att tolka eller generera CSV-filer f\xF6r att antingen\
  \ importera tabelldata fr\xE5n externa k\xE4llor eller exportera data f\xF6r anv\xE4\
  ndning i andra program."
title: Arbeta med CSV
weight: 37
---

## Hur:
JavaScript har inte inbyggd funktionalitet för att tolka eller omvandla CSV till strängar, som det har med JSON. Dock kan du enkelt hantera CSV-data genom att antingen använda rå JavaScript för enklare uppgifter eller använda kraftfulla bibliotek som `PapaParse` för mer komplexa scenarier.

### Grundläggande tolkning med rå JavaScript
För att tolka en enkel CSV-sträng till en array av objekt:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
Utskrift:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### Grundläggande generering till CSV med rå JavaScript
För att konvertera en array av objekt till en CSV-sträng:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Utskrift:

```
John,23,New York
Jane,28,Los Angeles
```

### Använda PapaParse för komplexa CSV-uppgifter
För mer komplexa scenarier är `PapaParse` ett robust bibliotek lämpligt för att tolka och omvandla CSV-filer med alternativ för strömmar, arbetare och hantering av stora filer.

Tolka CSV-fil eller sträng med PapaParse:

```javascript
// Efter att ha lagt till PapaParse i ditt projekt
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Tolkat:", results.data);
  }
});
```

Genererar:

```
Tolkat: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Omvandla en array till en CSV-sträng med PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Generera:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

Dessa exempel illustrerar grundläggande och avancerad hantering av CSV i JavaScript, vilket möjliggör enkel datautbyte i webbapplikationer och mer därtill.
