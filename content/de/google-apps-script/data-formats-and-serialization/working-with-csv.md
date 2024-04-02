---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:10.534955-07:00
description: "Das Arbeiten mit CSV-Dateien (Comma-Separated Values, kommagetrennte\
  \ Werte) in Google Apps Script umfasst das Lesen, Modifizieren und Schreiben von\u2026"
lastmod: '2024-03-13T22:44:53.358438-06:00'
model: gpt-4-0125-preview
summary: "Das Arbeiten mit CSV-Dateien (Comma-Separated Values, kommagetrennte Werte)\
  \ in Google Apps Script umfasst das Lesen, Modifizieren und Schreiben von\u2026"
title: Arbeiten mit CSV
weight: 37
---

## Was & Warum?

Das Arbeiten mit CSV-Dateien (Comma-Separated Values, kommagetrennte Werte) in Google Apps Script umfasst das Lesen, Modifizieren und Schreiben von Klartextdateien, wobei jede Zeile einen Datensatz darstellt, dessen Werte durch Kommas getrennt sind. Programmierer tun dies, um Daten leicht zwischen verschiedenen Anwendungen, Datenbanken oder Programmiersprachen auszutauschen, aufgrund der breiten Akzeptanz von CSV als einfaches, textbasiertes Datenaustauschformat.

## Wie zu:

### CSV-Daten lesen

Um CSV-Daten aus einer in Google Drive gespeicherten Datei zu lesen, müssen Sie zunächst den Inhalt der Datei als Zeichenkette abrufen und dann parsen. Google Apps Script erleichtert das Abrufen von Dateiinhalten mit dem DriveApp-Dienst.

```javascript
function readCSV() {
  var fileId = 'Ihre_Datei-ID_hier'; // Ersetzen mit der tatsächlichen Datei-ID
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Protokolliert die Zellen jeder Zeile
  }
}
```

### CSV-Daten schreiben

Das Erstellen und Schreiben in eine CSV-Datei erfordert das Konstruieren einer Zeichenkette mit kommagetrennten Werten und neuen Zeilen, dann das Speichern oder Exportieren dieser. Dieses Beispiel zeigt, wie eine neue CSV-Datei in Google Drive erstellt wird.

```javascript
function writeCSV() {
  var folderId = 'Ihre_Ordner-ID_hier'; // Ersetzen mit der ID des Drive-Ordners, wo die neue Datei erstellt wird
  var csvContent = "Name,Alter,Beruf\nJohn Doe,29,Ingenieur\nJane Smith,34,Designer";
  var fileName = "beispiel.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Beispiel-Ausgabe

Beim Protokollieren von Zeilenzellen aus dem Lesen einer CSV-Datei:

```plaintext
[John, 29, Ingenieur]
[Jane, 34, Designer]
```

Beim Schreiben wird eine Datei namens "beispiel.csv" mit folgendem Inhalt erstellt:

```plaintext
Name,Alter,Beruf
John Doe,29,Ingenieur
Jane Smith,34,Designer
```

## Tiefergehend

Historisch gesehen wurden CSV-Dateien aufgrund ihrer Einfachheit und menschlichen Lesbarkeit bevorzugt, was sie auch für Nicht-Programmierer zugänglich macht und für schnelle Dateninspektionen nützlich ist. Allerdings agiert Google Apps Script im Bereich des Google-Ökosystems, wo Google Sheets als leistungsfähige, benutzerfreundliche Alternative zur CSV-Manipulation dient. Sheets bietet nicht nur eine GUI zur Dateneditierung, sondern unterstützt auch komplexe Formeln, Styling und viele weitere Funktionen, die in rohen CSVs fehlen.

Trotz der von Google Sheets angebotenen Vorteile bleibt die direkte CSV-Manipulation in Google Apps Script wichtig für automatisierte Aufgaben, insbesondere beim Umgang mit externen Systemen, die Daten im CSV-Format generieren oder erfordern. Zum Beispiel bei der Integration mit Altsystemen, dem Export von Daten zur Verwendung in anderen Anwendungen oder der Vorverarbeitung vor dem Einspeisen von Daten in Google Sheets.

Darüber hinaus kann die Fähigkeit von Google Apps Script, mit CSV-Dateien zu arbeiten, mit dem Utilities-Dienst für erweiterte Codierungsanforderungen erweitert oder mit externen APIs für Umwandlungs-, Parsing- oder Validierungsaufgaben verbunden werden. Bei der Arbeit mit großen Datensätzen oder komplexen Manipulationen sollten jedoch die Google Sheets APIs in Betracht gezogen oder BigQuery für leistungsfähigere Datenverarbeitungsmöglichkeiten erkundet werden.

Während die Einfachheit ein Schlüsselgrund für die Beliebtheit von CSV bleibt, bieten diese Alternativen einen reichhaltigeren Funktionsumfang für den Umgang mit Daten im umfangreichen Google Cloud-Ökosystem.
