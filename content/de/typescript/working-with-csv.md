---
title:                "TypeScript: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien sind eine gängige Art, Daten in tabellarischer Form zu speichern. Sie werden häufig in den Bereichen Finanzen, Buchhaltung und Datenanalyse verwendet. Indem wir lernen, wie man mit CSV-Dateien in TypeScript arbeitet, können wir effizienter Daten verarbeiten und analysieren.

## So geht's

Das erste, was wir tun müssen, ist, die `csv-parser` Bibliothek zu installieren, die uns ermöglicht, CSV-Dateien in TypeScript zu verarbeiten. Wir können dies über die Node Package Manager (npm) tun, indem wir den Befehl `npm install csv-parser` im Terminal ausführen.

Als nächstes müssen wir in unserem TypeScript-Code die `csv-parser` Bibliothek importieren und die Funktion `parse()` verwenden, um die CSV-Datei zu lesen und in ein JavaScript-Objekt umzuwandeln. Im folgenden Beispiel lesen wir die Datei `meine_daten.csv` aus und geben die Daten in der Konsole aus:

```TypeScript
import * as csv from 'csv-parser';
import * as fs from 'fs';

fs.createReadStream('meine_daten.csv')
  .pipe(csv())
  .on('data', (data) => console.log(data))
  .on('end', () => console.log('CSV-Datei erfolgreich gelesen.'));
```

Das `data`-Event wird für jede Zeile der CSV-Datei ausgelöst und die entsprechenden Daten werden im `data`-Parameter übergeben. Wir können dann auf die einzelnen Werte zugreifen, indem wir den Namen der Spalte verwenden, z.B. `data['Name']` oder `data['Alter']`.

Als Ergebnis sollten wir in der Konsole jedes Mal ein Objekt mit den Daten einer Zeile sehen. Um die Daten in einer Datei zu speichern, können wir die Funktion `fs.writeFileSync()` verwenden.

## Tiefer gehend

Wir haben bisher nur gezeigt, wie man eine CSV-Datei ausliest und die Daten in der Konsole ausgibt. Aber was ist, wenn wir die Daten bearbeiten und in eine neue CSV-Datei schreiben wollen?

Eine Möglichkeit hierfür ist die Verwendung der `csv-writer` Bibliothek. Diese ermöglicht es uns, CSV-Dateien zu erstellen und zu bearbeiten. Wir können sie über npm installieren mit `npm install csv-writer`.

Im folgenden Codebeispiel erstellen wir eine neue CSV-Datei mit den Spalten "Name", "Alter" und "Beruf" und schreiben die Daten aus unserer ursprünglichen CSV-Datei `meine_daten.csv` in die neue Datei `verarbeitete_daten.csv`:

```TypeScript
import { createObjectCsvWriter } from 'csv-writer';
import * as csv from 'csv-parser';
import * as fs from 'fs';

fs.createReadStream('meine_daten.csv')
  .pipe(csv())
  .on('data', (data) => {
    // Daten bearbeiten und Felder für die neue Datei konfigurieren
    let name = data['Name'];
    let alter = data['Alter'];
    let beruf = data['Beruf'];

    let newData = {
      name: name + ' 2.0',
      age: alter,
      profession: beruf + ' (geändert)'
    };

    // neue Zeile mit bearbeiteten Daten in CSV schreiben
    csvWriter
      .writeRecords([newData])
      .then(() => console.log('Zeile erfolgreich in CSV-Datei geschrieben.'));
  })
  .on('end', () => console.log('CSV-Datei erfolgreich gelesen.'));

// CSV-Writer konfigurieren mit Spaltennamen
const csvWriter = createObjectCsvWriter({
  path: 'verarbeitete_daten.csv',
  header: [
    { id: 'name', title: 'Name' },
    { id: 'age', title: 'Alter' },
    { id: 'profession', title: 'Beruf' }
  ]
});
```

Diese Bibliothek bietet auch weitere Funktionen, wie z.B. das Sortieren und Filtern von Daten und das Erstellen von verschiedenen CSV-Formaten.

## Siehe auch

Weitere Informationen zu CSV-Dateien und deren Verarbeitung in TypeScript finden Sie hier:

- https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html (Offizielles TypeScript-Handbuch)
- https://csv.js.org/ (csv-parser-Dokumentation)
- https://csv.js.org/write.html (csv-writer-Dokumentation)