---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:27.664930-07:00
description: "Hoe te: De kernbibliotheek van Dart vereenvoudigt het parseren van datums\
  \ door de `DateTime` klasse. Voor eenvoudige gevallen waarin je het formaat van\
  \ de\u2026"
lastmod: '2024-03-13T22:44:50.515317-06:00'
model: gpt-4-0125-preview
summary: De kernbibliotheek van Dart vereenvoudigt het parseren van datums door de
  `DateTime` klasse.
title: Een datum ontleden uit een string
weight: 30
---

## Hoe te:
De kernbibliotheek van Dart vereenvoudigt het parseren van datums door de `DateTime` klasse. Voor eenvoudige gevallen waarin je het formaat van de datumstring kent, kun je de `DateTime.parse()` methode gebruiken. Echter, voor meer complexe scenario's of wanneer je te maken hebt met meerdere formaten, wordt het `intl` package, specifiek de `DateFormat` klasse, onmisbaar.

### Gebruikmakend van Dart Kernbibliotheek:
```dart
void main() {
  // Gebruikmakend van DateTime.parse()
  var datumString = "2023-10-31";
  var geparseerdeDatum = DateTime.parse(datumString);
  
  print(geparseerdeDatum); // 2023-10-31 00:00:00.000
}
```

### Het `intl` Package Gebruiken:
Voeg eerst het `intl` package toe aan je `pubspec.yaml` bestand:
```yaml
dependencies:
  intl: ^0.17.0
```
Importeer vervolgens het package en gebruik `DateFormat` voor het parseren:
```dart
import 'package:intl/intl.dart';

void main() {
  var datumString = "October 31, 2023";
  var datumFormaat = DateFormat("MMMM dd, yyyy");
  var geparseerdeDatum = datumFormaat.parse(datumString);
  
  print(geparseerdeDatum); // 2023-10-31 00:00:00.000
}
```
Het `intl` package biedt robuuste opties voor het parseren van datums, waardoor verschillende internationale datumformaten naadloos kunnen worden gehanteerd.
