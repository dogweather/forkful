---
title:                "Das Arbeiten mit csv"
html_title:           "TypeScript: Das Arbeiten mit csv"
simple_title:         "Das Arbeiten mit csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals mit großen Mengen von Daten gearbeitet hast, hast du wahrscheinlich schonmal von CSV-Dateien gehört. CSV steht für "Comma-Separated Values" und ist ein einfaches Format zur Speicherung von tabellarischen Daten. In diesem Artikel werde ich dir zeigen, wie du mit CSV-Dateien in TypeScript arbeiten kannst und welche Vorteile es bringen kann.

## Wie geht das?

Um mit CSV-Dateien in TypeScript zu arbeiten, gibt es zwei Ansätze: manuell oder mit Hilfe einer Bibliothek. Zunächst schauen wir uns die manuelle Methode an.

### Manuell

Die manuelle Methode besteht aus drei Schritten: Öffnen der CSV-Datei, Lesen der Daten und Verarbeiten der Daten. Wir werden diese Schritte anhand eines einfachen Beispiels zeigen.

Wir haben eine CSV-Datei mit den Daten von Schülern und ihren Noten:

```TypeScript
const fs = require('fs')

// Öffnen der CSV-Datei
const data = fs.readFileSync('studenten.csv', 'utf-8')

// Lesen der Daten
const lines = data.split('\n')

// Verarbeiten der Daten
for (let line of lines) {
  const columns = line.split(',')
  const name = columns[0]
  const grade = columns[1]
  console.log(`${name} hat eine Note von ${grade}`)
}
```

Wenn wir dieses Skript ausführen, erhalten wir die folgende Ausgabe:

```
Max hat eine Note von 1.2
Anna hat eine Note von 2.0
Paul hat eine Note von 1.0
```

Diese Methode ist zwar einfach, aber auch fehleranfällig. Wenn die CSV-Datei unerwartete Zeichen oder Formatierungen enthält, kann dies zu Fehlern führen.

### Mit Bibliotheken

Eine bessere Alternative ist die Verwendung von Bibliotheken, die speziell für das Lesen von CSV-Dateien entwickelt wurden. Eine beliebte Bibliothek ist `csv-parse`, die wir in unserem TypeScript-Projekt installieren können:

```
npm install csv-parse
```

Anschließend können wir die Bibliothek in unserem Skript importieren und verwenden:

```TypeScript
import parse from 'csv-parse'
import fs from 'fs'

// Öffnen der CSV-Datei
const data = fs.readFileSync('studenten.csv', 'utf-8')

// Lesen der Daten mit csv-parse
parse(data, { delimiter: ',' }, (err, output) => {
  if (err) {
    console.log(err)
    return
  }

  // Verarbeiten der Daten
  for (let line of output) {
    const name = line[0]
    const grade = line[1]
    console.log(`${name} hat eine Note von ${grade}`)
  }
})
```

Das Ergebnis ist das gleiche wie in der manuellen Methode, aber diesmal haben wir einen besseren Schutz gegen unerwartete Daten.

## Deep Dive

Wenn du tiefer in die Arbeit mit CSV-Dateien einsteigen möchtest, solltest du dir die Dokumentation der von dir verwendeten Bibliothek ansehen. Dort findest du weitere Funktionen und Optionen, die dir helfen können, CSV-Dateien effizienter zu verarbeiten.

## Siehe auch

- [csv-parse Dokumentation](https://csv.js.org/parse/)
- [Papa Parse Dokumentation](https://www.papaparse.com/)