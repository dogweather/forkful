---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z CSV (Comma-Separated Values) polega na manipulowaniu danymi zapisanymi w formacie tekstowym oddzielonym przecinkami. Programiści robią to, aby łatwo wymieniać i przetwarzać dane pomiędzy różnymi aplikacjami oraz systemami.

## Jak to zrobić:
```TypeScript
import * as fs from 'fs';
import * as path from 'path';
import csvParse from 'csv-parse/lib/sync';

// Czytanie CSV
const csvFilePath = path.resolve(__dirname, 'dane.csv');
const csvData = fs.readFileSync(csvFilePath, { encoding: 'utf-8' });

// Parsowanie CSV
const records = csvParse(csvData, {
  columns: true,
  skip_empty_lines: true
});

// Wyświetlanie danych
console.log(records);

// Zapisywanie danych do nowego CSV
import { stringify } from 'csv-stringify/sync';

const output = stringify(records, { header: true });
fs.writeFileSync(path.resolve(__dirname, 'wyniki.csv'), output);

// Sample output: wypisuje tablicę obiektów z danych CSV
```

## Deep Dive
Format CSV pojawił się w latach 70 i szybko stał się standardem wymiany danych między systemami. Alternatywami CSV są JSON, XML czy bazy danych SQL i NoSQL. Praca z CSV w TypeScript wymaga często dodatkowych bibliotek, takich jak `csv-parse` czy `csv-stringify`, ponieważ TypeScript nie ma wbudowanego wsparcia dla CSV.

## See Also
- Specyfikacja CSV: https://tools.ietf.org/html/rfc4180
- Pakiet `csv-parse`: https://csv.js.org/parse/
- Pakiet `csv-stringify`: https://csv.js.org/stringify/
- Przewodnik po TypeScript: https://www.typescriptlang.org/docs/
