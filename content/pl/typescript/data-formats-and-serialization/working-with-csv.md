---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:31.509667-07:00
description: "Jak to zrobi\u0107: W TypeScript mo\u017Cesz pracowa\u0107 z plikami\
  \ CSV za pomoc\u0105 kodu natywnego lub wykorzystuj\u0105c biblioteki stron trzecich,\
  \ takie jak `csv-parser` do\u2026"
lastmod: '2024-03-13T22:44:35.162985-06:00'
model: gpt-4-0125-preview
summary: "W TypeScript mo\u017Cesz pracowa\u0107 z plikami CSV za pomoc\u0105 kodu\
  \ natywnego lub wykorzystuj\u0105c biblioteki stron trzecich, takie jak `csv-parser`\
  \ do odczytu i `csv-writer` do zapisu plik\xF3w CSV."
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:
W TypeScript możesz pracować z plikami CSV za pomocą kodu natywnego lub wykorzystując biblioteki stron trzecich, takie jak `csv-parser` do odczytu i `csv-writer` do zapisu plików CSV.

### Odczyt CSV z `csv-parser`
Najpierw zainstaluj `csv-parser` za pomocą npm:

```
npm install csv-parser
```

Następnie odczytaj plik CSV w następujący sposób:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Output: Tablica obiektów, każdy reprezentuje wiersz w CSV
  });
```

Zakładając, że `data.csv` zawiera:

```
name,age
Alice,30
Bob,25
```

Wynik będzie następujący:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Zapis do CSV z `csv-writer`
Aby zapisać do pliku CSV, najpierw zainstaluj `csv-writer`:

```
npm install csv-writer
```

Następnie użyj go w następujący sposób:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAZWA'},
    {id: 'age', title: 'WIEK'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('Plik CSV został pomyślnie zapisany'));
```

Ten kod zapisuje do `out.csv`:

```
NAZWA,WIEK
Alice,30
Bob,25
```

Te przykłady pokazują, jak efektywnie integrować przetwarzanie CSV w Twoich projektach TypeScript, czy to dla analizy danych, czy dla zewnętrznego przechowywania danych aplikacji.
