---
title:                "Arbeiten mit CSV"
aliases:
- de/javascript/working-with-csv.md
date:                  2024-02-03T19:20:21.740294-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Mit CSV (Comma-Separated Values, durch Kommata getrennte Werte) in JavaScript zu arbeiten, bedeutet, CSV-Dateien zu parsen oder zu generieren, um Tabellendaten aus externen Quellen zu importieren oder Daten für die Verwendung in anderen Programmen zu exportieren. Programmierer machen dies, weil es einen einfachen, leichtgewichtigen Datenaustausch zwischen Anwendungen, Datenbanken und Systemen ermöglicht, wo komplexere Formate wie JSON eventuell zu aufwendig wären.

## Wie geht das:
JavaScript verfügt nicht über integrierte CSV-Parsing- oder Stringifying-Funktionen wie bei JSON. Jedoch kann man CSV-Daten leicht verwalten, indem man entweder rohes JavaScript für einfachere Aufgaben verwendet oder leistungsfähige Bibliotheken wie `PapaParse` für komplexere Szenarien nutzt.

### Basis-Parsing mit rohem JavaScript
Um einen einfachen CSV-String in ein Array von Objekten zu parsen:

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
Ausgabe:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### Einfache Erzeugung zu CSV mit rohem JavaScript
Um ein Array von Objekten in einen CSV-String zu konvertieren:

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

Ausgabe:

```
John,23,New York
Jane,28,Los Angeles
```

### Verwendung von PapaParse für komplexe CSV-Aufgaben
Für komplexere Szenarien ist `PapaParse` eine robuste Bibliothek, die sich für das Parsen und Stringifizieren von CSV-Dateien eignet, mit Optionen für Streams, Worker und die Handhabung großer Dateien.

Parsen einer CSV-Datei oder eines CSV-Strings mit PapaParse:

```javascript
// Nachdem PapaParse zu Ihrem Projekt hinzugefügt wurde
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Geparst:", results.data);
  }
});
```

Erzeugt:

```
Geparst: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Stringifizieren eines Arrays zu einem CSV-String mit PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Erzeugt:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

Diese Beispiele veranschaulichen die grundlegende und fortgeschrittene Handhabung von CSV in JavaScript, was den Datenaustausch in Webanwendungen und darüber hinaus erleichtert.
