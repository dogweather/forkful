---
title:                "Arbeiten mit CSV."
html_title:           "Javascript: Arbeiten mit CSV."
simple_title:         "Arbeiten mit CSV."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

Was ist CSV und warum verwenden Programmierer es?

CSV steht für "Comma-Separated Values" und ist ein Dateiformat, das weit verbreitet ist, um strukturierte Daten zu speichern und auszutauschen. Es besteht aus einfachen Textdaten, die durch Kommas getrennt sind. Programmierer verwenden CSV, um Daten in einem einfachen, lesbaren Format zu speichern und zu verarbeiten, daher ist es ein beliebtes Format für Tabellen und Datenbanken.

Wie funktioniert es?

```Javascript
// Beispiel-Daten in CSV-Format
var csvData = '"Name","Alter","Wohnort"\n"Max Mustermann",30,"Berlin"\n"Lisa Müller",25,"Hamburg"';

// CSV in ein zweidimensionales Array konvertieren
var arrayData = [];
csvData.split('\n').forEach(function(row) {
  var rowData = row.split(',');
  arrayData.push(rowData);
});

// Array ausgeben
console.log(arrayData);

// Ausgabe:
// [ [ 'Name', 'Alter', 'Wohnort' ],
// [ 'Max Mustermann', '30', 'Berlin' ],
// [ 'Lisa Müller', '25', 'Hamburg' ] ]
```

Hier verwenden wir die `split()` Funktion, um die einzelnen Zeilen in unserem CSV-String zu trennen und dann `forEach()` um jede Zeile in ein Array zu konvertieren. Am Ende haben wir ein zweidimensionales Array mit unseren Daten, das wir weiterverarbeiten können.

Tiefere Einblicke

CSV existiert bereits seit den 1970er Jahren und wurde ursprünglich für den Austausch von Tabellendaten zwischen verschiedenen Computersystemen entwickelt. In der Vergangenheit wurden auch andere Formate wie XML oder JSON für diesen Zweck verwendet, aber aufgrund der einfachen Struktur und der weit verbreiteten Unterstützung ist CSV immer noch ein beliebtes Format.

Alternativen zu CSV sind beispielsweise JSON, XML oder das Excel-Format. Jedes davon hat seine eigenen Vor- und Nachteile, je nach den Anforderungen und der Verwendungszweck des Projekts.

Für die Arbeit mit CSV gibt es viele Bibliotheken und Frameworks in verschiedenen Programmiersprachen, die das Einlesen, Verarbeiten und Schreiben von CSV-Dateien erleichtern. In JavaScript gibt es zum Beispiel die Bibliothek "csv-parse", die viele nützliche Funktionen bietet.

Weiterführende Links:

- Wikipedia-Artikel zu CSV: https://de.wikipedia.org/wiki/CSV_(Dateiformat)
- Liste der CSV-Bibliotheken für JavaScript: https://github.com/mafintosh/csv-parser#alternative-to-marvinj/csv-parser
- Offizielle Dokumentation von "csv-parse": https://csv.js.org/parse/