---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-separated values". Es ist ein einfaches Dateiformat, um strukturierte Daten zu speichern und zu teilen. Programmierer verwenden CSV, weil es leicht zu lesen, zu schreiben und in verschiedenen Programmen zu importieren ist.

## So geht's:
Um in JavaScript mit CSVs zu arbeiten, verwendest du am besten eine Bibliothek wie `PapaParse`. Hier ein einfaches Beispiel, wie man eine CSV-Datei einliest:

```Javascript
const Papa = require('papaparse');
const fs = require('fs');

const csvFile = 'example.csv';
const csvData = fs.readFileSync(csvFile, 'utf8');

Papa.parse(csvData, {
  complete: function(results) {
    console.log(results.data);
  }
});
```

Stell dir `example.csv` so vor:

```
name,age
Alice,25
Bob,30
```

Das im Codeblock stehende Skript würde folgendes ausgeben:

```
[
  ["name", "age"],
  ["Alice", "25"],
  ["Bob", "30"]
]
```

## Deep Dive
Die Idee von CSV ist so alt wie die ersten Computer, auch wenn das Format erst später standardisiert wurde. Eine Alternative zu CSV ist zum Beispiel JSON, das besser für komplexe Daten ist. Beim Arbeiten mit CSV in JavaScript musst du auf Zeilenumbrüche, Kommas in Daten und Zeichenkodierung achten, um Fehler zu vermeiden.

## See Also
- PapaParse Dokumentation: https://www.papaparse.com/docs
- Node.js fs Module: https://nodejs.org/api/fs.html
- CSV Standard: https://tools.ietf.org/html/rfc4180
- JSON als Alternative: https://www.json.org/json-de.html
