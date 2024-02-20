---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:27.997360-07:00
description: "Die Arbeit mit CSV (Comma-Separated Values, zu Deutsch: kommagetrennte\
  \ Werte) beinhaltet das Lesen von und Schreiben in CSV-Dateien, einem g\xE4ngigen\u2026"
lastmod: 2024-02-19 22:05:12.577104
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV (Comma-Separated Values, zu Deutsch: kommagetrennte Werte)\
  \ beinhaltet das Lesen von und Schreiben in CSV-Dateien, einem g\xE4ngigen\u2026"
title: Arbeiten mit CSV
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV (Comma-Separated Values, zu Deutsch: kommagetrennte Werte) beinhaltet das Lesen von und Schreiben in CSV-Dateien, einem gängigen Datenaustauschformat, das aufgrund seiner Einfachheit und breiten Unterstützung über verschiedene Plattformen und Sprachen hinweg verwendet wird. Programmierer beschäftigen sich mit CSV-Dateien, um Daten von Anwendungen, Datenbanken und Diensten zu importieren oder zu exportieren, was die einfache Manipulation und das Teilen von Daten ermöglicht.

## Wie:

In TypeScript können Sie mit CSV-Dateien durch nativen Code oder durch die Nutzung von Drittanbieter-Bibliotheken wie `csv-parser` für das Lesen und `csv-writer` für das Schreiben von CSV-Dateien arbeiten.

### CSV lesen mit `csv-parser`

Zuerst installieren Sie `csv-parser` via npm:

```
npm install csv-parser
```

Lesen Sie dann eine CSV-Datei wie folgt:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Ausgabe: Array von Objekten, jedes repräsentiert eine Zeile in der CSV
  });
```

Angenommen, `data.csv` enthält:

```
name,age
Alice,30
Bob,25
```

Die Ausgabe wird sein:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### CSV schreiben mit `csv-writer`

Um in eine CSV-Datei zu schreiben, installieren Sie zuerst `csv-writer`:

```
npm install csv-writer
```

Verwenden Sie es dann wie folgt:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('Die CSV-Datei wurde erfolgreich geschrieben'));
```

Dieser Code schreibt folgendes in `out.csv`:

```
NAME,AGE
Alice,30
Bob,25
```

Diese Beispiele zeigen, wie Sie die CSV-Verarbeitung in Ihren TypeScript-Projekten effizient integrieren können, egal ob es um das Lesen von Daten für die Analyse geht oder um das externes Speichern von Anwendungsdaten.
