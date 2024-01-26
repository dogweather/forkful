---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV, eller "kommadelte verdier", er et format brukt for å lagre tabelldata enkelt og greit. Programmerere bruker CSV fordi det er lettleselig for både mennesker og maskiner, og støttes av de fleste databehandlingsverktøy.

## Slik gjør du:
Her er hvordan du kan jobbe med CSV-filer i TypeScript:

```TypeScript
import fs from 'fs';
import parse from 'csv-parse/lib/sync';

const csvFileContent = fs.readFileSync('data.csv', 'utf8');

// Synkron parsing av CSV-innhold til et 2D array
const records = parse(csvFileContent, {
    columns: true,
    skip_empty_lines: true,
});

// Logge resultatet
console.log(records);
```

Om `data.csv` inneholder:
```
name,age
Alice,25
Bob,30
```

Vil output bli:
```TypeScript
[
  { name: 'Alice', age: '25' },
  { name: 'Bob', age: '30' }
]
```

## Dypdykk
CSV-formatet har vært i bruk siden 1970-tallet og er et enkelt, tekstbasert format. Alternativer som JSON eller XML tilbyr mer kompleksitet og struktur for datarepresentasjon. Når du jobber med CSV i TypeScript, bør det tas hensyn til riktig tekstkoding (vanligvis UTF-8), escaping av spesialtegn og konvertering av datatyper fra strenger til passende format.

## Se også
- [RFC 4180](https://tools.ietf.org/html/rfc4180), standarden for CSV.
- [Papaparse](https://www.papaparse.com/), en kraftig CSV-parser for nettleseren.
- [D3-dsv](https://github.com/d3/d3-dsv), et JavaScript-bibliotek for å parse og formatere dsv (inkludert csv) data.
