---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:28.180583-07:00
description: "Die Arbeit mit CSV-Dateien (Comma Separated Values) beinhaltet das Parsen\
  \ und Generieren von Textdateien, bei denen jede Zeile Werte enth\xE4lt, die durch\u2026"
lastmod: '2024-03-13T22:44:53.605715-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV-Dateien (Comma Separated Values) beinhaltet das Parsen\
  \ und Generieren von Textdateien, bei denen jede Zeile Werte enth\xE4lt, die durch\u2026"
title: Arbeiten mit CSV
weight: 37
---

## Was & Warum?

Die Arbeit mit CSV-Dateien (Comma Separated Values) beinhaltet das Parsen und Generieren von Textdateien, bei denen jede Zeile Werte enthält, die durch Kommas getrennt sind. Programmierer tun dies, um den Datenaustausch zwischen verschiedenen Anwendungen zu ermöglichen oder um die Datenspeicherung in einem leichten, menschenlesbaren Format zu erleichtern.

## Wie geht das:

Um CSV-Dateien in Dart zu handhaben, verarbeitet man den Text entweder manuell oder verwendet Drittanbieterbibliotheken, um die Aufgabe zu vereinfachen. Hier sehen wir uns beide Ansätze an.

### Manuelles Parsen von CSV

Wenn Ihre Anforderungen einfach sind, könnten Sie sich dafür entscheiden, einen CSV-String manuell zu parsen. Dies kann mit den Kern-String-Manipulationsfunktionen von Dart erreicht werden:

```dart
void main() {
  // Beispiel CSV-Daten
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Aufteilen der CSV-Daten in Zeilen
  List<String> lines = csvData.split('\n');
  
  // Parsen jeder Zeile
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // Ausgabe der geparsten Daten
  print(data);
}

// Beispiel-Ausgabe:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Verwendung einer Drittanbieterbibliothek: `csv`

Für komplexere Szenarien oder um Ihren Code zu vereinfachen, können Sie eine beliebte Drittanbieterbibliothek wie `csv` verwenden. Fügen Sie sie zuerst Ihrem Projekt hinzu, indem Sie `csv: ^5.0.0` (oder die neueste Version) in Ihrer `pubspec.yaml`-Datei unter `dependencies` einfügen. Verwenden Sie es dann wie folgt:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Verwendung des CsvToListConverter zum Parsen der CSV-Daten
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Das erste Listenelement enthält die Kopfzeilen
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // Entfernen der Kopfzeile vor der weiteren Verarbeitung
  listData.removeAt(0);
  
  // Umwandlung in List<Map<String, dynamic>> für ein strukturierteres Format
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Ausgabe der zugeordneten Daten
  print(mappedData);
}

// Beispiel-Ausgabe:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Beide Methoden demonstrieren, wie man mit CSV-Daten arbeitet: die erste manuell, zu Lernzwecken oder wenn man es mit sehr einfachen CSV-Strukturen zu tun hat; die zweite, indem man eine leistungsfähige Bibliothek nutzt, die das Parsen vereinfacht und verschiedene Komplexitäten der CSV-Formatierung handhaben kann.
