---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:31.509667-07:00
description: "Praca z CSV (Comma-Separated Values, czyli warto\u015Bciami oddzielonymi\
  \ przecinkami) obejmuje odczytywanie z plik\xF3w CSV oraz zapisywanie do nich, co\
  \ jest\u2026"
lastmod: '2024-03-13T22:44:35.162985-06:00'
model: gpt-4-0125-preview
summary: "Praca z CSV (Comma-Separated Values, czyli warto\u015Bciami oddzielonymi\
  \ przecinkami) obejmuje odczytywanie z plik\xF3w CSV oraz zapisywanie do nich, co\
  \ jest powszechnym formatem wymiany danych u\u017Cywanym ze wzgl\u0119du na jego\
  \ prostot\u0119 i szerokie wsparcie na r\xF3\u017Cnych platformach i w r\xF3\u017C\
  nych j\u0119zykach."
title: Praca z plikami CSV
weight: 37
---

## Co i dlaczego?

Praca z CSV (Comma-Separated Values, czyli wartościami oddzielonymi przecinkami) obejmuje odczytywanie z plików CSV oraz zapisywanie do nich, co jest powszechnym formatem wymiany danych używanym ze względu na jego prostotę i szerokie wsparcie na różnych platformach i w różnych językach. Programiści zajmują się plikami CSV, aby importować lub eksportować dane z aplikacji, baz danych i usług, umożliwiając łatwą manipulację danymi i ich udostępnianie.

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
