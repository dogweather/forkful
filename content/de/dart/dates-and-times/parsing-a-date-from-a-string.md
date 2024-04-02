---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:13.749231-07:00
description: "Das Parsen eines Datums aus einer Zeichenkette in Dart beinhaltet die\
  \ Umwandlung von textueller Darstellung von Daten und Zeiten in ein `DateTime`-Objekt.\u2026"
lastmod: '2024-03-13T22:44:53.591304-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einer Zeichenkette in Dart beinhaltet die Umwandlung\
  \ von textueller Darstellung von Daten und Zeiten in ein `DateTime`-Objekt.\u2026"
title: Einen Datum aus einem String analysieren
weight: 30
---

## Was & Warum?
Das Parsen eines Datums aus einer Zeichenkette in Dart beinhaltet die Umwandlung von textueller Darstellung von Daten und Zeiten in ein `DateTime`-Objekt. Diese Operation ist essentiell für Anwendungen, die sich mit Planung, Datenanalyse oder jeder Funktion, die Datummanipulation benötigt, befassen, und stellt sicher, dass datumsbezogene Daten korrekt verstanden und vom Programm verarbeitet werden.

## Wie:
Die Kernbibliothek von Dart vereinfacht das Parsen von Daten durch die `DateTime`-Klasse. Für einfache Fälle, in denen Sie das Format des Datumsstrings kennen, können Sie die Methode `DateTime.parse()` verwenden. Bei komplexeren Szenarien oder wenn Sie es mit mehreren Formaten zu tun haben, wird allerdings das `intl`-Paket, speziell die `DateFormat`-Klasse, unverzichtbar.

### Verwendung der Dart-Kernbibliothek:
```dart
void main() {
  // Verwendung von DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Verwendung des `intl`-Pakets:
Fügen Sie zunächst das `intl`-Paket zu Ihrer `pubspec.yaml`-Datei hinzu:
```yaml
dependencies:
  intl: ^0.17.0
```
Importieren Sie dann das Paket und verwenden Sie `DateFormat` zum Parsen:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
Das `intl`-Paket bietet robuste Optionen für das Parsen von Daten, die die nahtlose Behandlung verschiedener internationaler Datenformate ermöglichen.
