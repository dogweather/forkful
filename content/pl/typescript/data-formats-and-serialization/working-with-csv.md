---
title:                "Praca z plikami CSV"
aliases:
- pl/typescript/working-with-csv.md
date:                  2024-02-03T19:21:31.509667-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
