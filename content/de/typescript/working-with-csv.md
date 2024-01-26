---
title:                "Arbeiten mit CSV-Dateien"
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für Comma-Separated Values, also Daten, die durch Kommas getrennt sind. Programmierer nutzen CSV-Formate, um einfach mit Tabellendaten zu arbeiten, da sie leicht zu lesen, zu schreiben und mit verschiedenen Programmen kompatibel sind.

## How to:
Hier ist der Code, um eine CSV-Datei zu lesen und zu schreiben.

```TypeScript
import fs from 'fs';
import { parse, unparse } from 'papaparse';

// Eine CSV-Datei lesen
const csvFileContent = fs.readFileSync('meine-daten.csv', 'utf8');
const parsedData = parse(csvFileContent, {
  header: true
});
console.log(parsedData.data);

// Eine CSV-Datei schreiben
const dataToWrite = [
  { name: 'Max', age: 29 },
  { name: 'Moritz', age: 35 }
];
const unparsedData = unparse(dataToWrite);
fs.writeFileSync('aktualisierte-daten.csv', unparsedData);
```

Beispiel-Ausgabe beim Lesen:

```TypeScript
[
  { "name": "Max", "age": "29" },
  { "name": "Moritz", "age": "35" }
]
```

## Deep Dive
CSV-Datenformate gibt es schon lange, sogar vor Computern. Sie sind weniger komplex als XML oder JSON und sehr gut geeignet für große Datenmengen. Alternativen zu CSV sind z.B. Excel-Formate (XLSX) oder Datenbanken, aber diese können überdimensioniert sein für simple Datenmanipulation. Bei der Arbeit mit CSV in TypeScript ist es wichtig, auf saubere Fehlerbehandlung und Zeichensatz-Kompatibilität zu achten.

## See Also
- PapaParse Dokumentation: https://www.papaparse.com/docs
- Node.js Dateisystem (fs) Modul: https://nodejs.org/api/fs.html
- CSV in der Praxis: https://www.ietf.org/rfc/rfc4180.txt
