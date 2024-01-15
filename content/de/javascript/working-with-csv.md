---
title:                "Arbeiten mit csv"
html_title:           "Javascript: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

# Warum

CSV-Dateien sind ein gängiges Format, um Daten zu speichern und auszutauschen. Sie können in verschiedenen Programmen geöffnet und verarbeitet werden und sind daher sehr praktisch für Entwickler und Datenanalysten. Mit Javascript können wir mithilfe von Bibliotheken schnell und einfach mit CSV-Dateien arbeiten und sie in unsere Projekte integrieren.

# Wie es geht

Um mit CSV-Dateien in Javascript zu arbeiten, gibt es verschiedene Bibliotheken, die uns dabei helfen können. Ein Beispiel dafür ist "csvtojson", mit dem wir CSV-Dateien in ein JSON-Objekt konvertieren können. Hier ist ein einfaches Beispiel:

```Javascript
const csv = require('csvtojson')

csv()
.fromFile('data.csv')
.then((jsonObj) => {
  console.log(jsonObj)
})
```

Dieses Code-Beispiel lädt eine CSV-Datei mit dem Namen "data.csv" und konvertiert sie in ein JSON-Objekt. Anschließend wird das Ergebnis in der Konsole ausgegeben. Die Ausgabe wird in der folgenden Form sein:

```Javascript
[
  { Name: 'John', Alter: '26', Beruf: 'Software Entwickler' },
  { Name: 'Lisa', Alter: '30', Beruf: 'Projekt Manager' },
  { Name: 'Tom', Alter: '32', Beruf: 'Datenanalyst' }
]
```

Dies ist nur ein einfaches Beispiel für die Verwendung von "csvtojson". Es gibt noch viele weitere Funktionen und Optionen, die es uns ermöglichen, CSV-Dateien auf verschiedene Arten zu verarbeiten.

# Tiefere Einblicke

Manchmal kann es notwendig sein, CSV-Dateien manuell zu manipulieren, anstatt sie einfach in ein JSON-Objekt zu konvertieren. Dafür haben wir verschiedene Methoden zur Verfügung, wie zum Beispiel "readline" oder "fs" aus dem Node.js Core-Modul. Diese Methoden ermöglichen es uns, die CSV-Datei Zeile für Zeile zu lesen und spezifische Manipulationen durchzuführen, wie zum Beispiel das Hinzufügen oder Löschen von Zeilen oder das Ändern von Daten.

Außerdem können wir mithilfe von Regex-Patterns CSV-Dateien nach bestimmten Mustern durchsuchen und daraus Daten extrahieren. Dies kann besonders nützlich sein, wenn wir große CSV-Dateien mit vielen Spalten haben und nur bestimmte Daten davon benötigen.

Für eine umfassendere und detaillierte Anleitung zur Arbeit mit CSV-Dateien in Javascript empfehlen wir die offizielle Dokumentation von Node.js sowie die Dokumentationen von spezifischen Bibliotheken wie "csvtojson".

# Siehe auch

- [Offizielle Dokumentation von Node.js](https://nodejs.org/en/docs/)
- [Dokumentation von "csvtojson"](https://csv.js.org/converter/)
- [Einfache CSV-Verarbeitung mit Javascript](https://medium.com/@binyamin/simple-csv-manipulation-with-javascript-using-regex-parsing-a1f5e826ef06)