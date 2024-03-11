---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:11.758090-07:00
description: "Werken met CSV (Comma Separated Values) bestanden betreft het parseren\
  \ en genereren van tekstbestanden waarbij elke regel waarden bevat die door komma's\u2026"
lastmod: '2024-03-11T00:14:24.345360-06:00'
model: gpt-4-0125-preview
summary: "Werken met CSV (Comma Separated Values) bestanden betreft het parseren en\
  \ genereren van tekstbestanden waarbij elke regel waarden bevat die door komma's\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV (Comma Separated Values) bestanden betreft het parseren en genereren van tekstbestanden waarbij elke regel waarden bevat die door komma's zijn gescheiden. Programmeurs doen dit om gegevensuitwisseling tussen verschillende applicaties mogelijk te maken of om gegevensopslag in een lichtgewicht, voor mensen leesbaar formaat te vergemakkelijken.

## Hoe:

Om CSV-bestanden in Dart te verwerken, verwerk je de tekst meestal handmatig of gebruik je externe bibliotheken om de taak te vereenvoudigen. Hier kijken we naar beide benaderingen.

### Handmatig CSV Parsen

Als je behoeften eenvoudig zijn, kun je ervoor kiezen om een CSV-string handmatig te parsen. Dit kan worden bereikt met behulp van Dart's kernstringmanipulatiefuncties:

```dart
void main() {
  // Voorbeeld CSV-gegevens
  String csvData = "Naam,Leeftijd,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // De CSV-gegevens opsplitsen in regels
  List<String> regels = csvData.split('\n');
  
  // Elke regel parsen
  List<Map<String, String>> gegevens = [];
  List<String> koppen = regels.first.split(',');
  
  for (var i = 1; i < regels.length; i++) {
    List<String> rij = regels[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < koppen.length; j++) {
      record[koppen[j]] = rij[j];
    }
    gegevens.add(record);
  }
  
  // De geparseerde gegevens uitvoeren
  print(gegevens);
}

// Voorbeelduitvoer:
// [{Naam: John Doe, Leeftijd: 30, Email: john@example.com}, {Naam: Jane Smith, Leeftijd: 25, Email: jane@example.com}]
```

### Een Externe Bibliotheek Gebruiken: `csv`

Voor complexere scenario's of om je code te vereenvoudigen, kun je een populaire externe bibliotheek zoals `csv` gebruiken. Voeg deze eerst toe aan je project door `csv: ^5.0.0` (of de nieuwste versie) op te nemen in je `pubspec.yaml`-bestand onder `dependencies`. Gebruik het vervolgens als volgt:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Naam,Leeftijd,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Gebruik de CsvToListConverter om de CSV-gegevens te parsen
  List<List<dynamic>> lijstGegevens = const CsvToListConverter().convert(csvData);
  
  // Het eerste lijstelement bevat de koppen
  List<String> koppen = lijstGegevens.first.map((item) => item.toString()).toList();
  
  // Verwijder de koprij voordat je verder gaat met verwerken
  lijstGegevens.removeAt(0);
  
  // Converteren naar List<Map<String, dynamic>> voor een meer gestructureerd formaat
  List<Map<String, dynamic>> gemapteGegevens = lijstGegevens.map((lijst) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < koppen.length; i++) {
      map[koppen[i]] = lijst[i];
    }
    return map;
  }).toList();
  
  // De gemapte gegevens uitvoeren
  print(gemapteGegevens);
}

// Voorbeelduitvoer:
// [{Naam: John Doe, Leeftijd: 30, Email: john@example.com}, {Naam: Jane Smith, Leeftijd: 25, Email: jane@example.com}]
```

Beide methoden laten zien hoe je met CSV-gegevens kunt werken: de eerste handmatig, voor leerdoeleinden of bij het omgaan met zeer eenvoudige CSV-structuren; de tweede, door gebruik te maken van een krachtige bibliotheek die het parseren vereenvoudigt en kan omgaan met verschillende complexiteiten van CSV-formaten.
