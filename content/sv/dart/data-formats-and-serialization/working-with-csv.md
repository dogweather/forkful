---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:13.888714-07:00
description: "Hur man g\xF6r: F\xF6r att hantera CSV-filer i Dart, brukar man antingen\
  \ manuellt bearbeta texten eller anv\xE4nda tredjepartsbibliotek f\xF6r att f\xF6\
  renkla uppgiften.\u2026"
lastmod: '2024-03-13T22:44:37.636395-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att hantera CSV-filer i Dart, brukar man antingen manuellt bearbeta\
  \ texten eller anv\xE4nda tredjepartsbibliotek f\xF6r att f\xF6renkla uppgiften."
title: Arbeta med CSV
weight: 37
---

## Hur man gör:
För att hantera CSV-filer i Dart, brukar man antingen manuellt bearbeta texten eller använda tredjepartsbibliotek för att förenkla uppgiften. Här kommer vi att titta på båda metoder.

### Manuell tolkning av CSV
Om dina behov är enkla, kan du välja att manuellt tolka en CSV-sträng. Detta kan uppnås med hjälp av Darts kärnsträngmanipuleringsfunktioner:

```dart
void main() {
  //Exempel på CSV-data
  String csvData = "Namn,Ålder,E-post\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Delar upp CSV-datan i rader
  List<String> rader = csvData.split('\n');
  
  // Tolkar varje rad
  List<Map<String, String>> data = [];
  List<String> huvuden = rader.first.split(',');
  
  for (var i = 1; i < rader.length; i++) {
    List<String> rad = rader[i].split(',');
    Map<String, String> post = {};
    for (var j = 0; j < huvuden.length; j++) {
      post[huvuden[j]] = rad[j];
    }
    data.add(post);
  }
  
  // Skriver ut den tolkade datan
  print(data);
}

// Exempel på utdata:
// [{Namn: John Doe, Ålder: 30, E-post: john@example.com}, {Namn: Jane Smith, Ålder: 25, E-post: jane@example.com}]
```

### Använda ett tredjepartsbibliotek: `csv`
För mer komplexa scenarier eller för att förenkla din kod, kan du använda ett populärt tredjepartsbibliotek som `csv`. Lägg först till det i ditt projekt genom att inkludera `csv: ^5.0.0` (eller senaste versionen) i din `pubspec.yaml`-fil under `dependencies`. Använd det sedan enligt följande:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Namn,Ålder,E-post\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Använder CsvToListConverter för att tolka CSV-datan
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Det första listobjektet innehåller huvuden
  List<String> huvuden = listData.first.map((item) => item.toString()).toList();
  
  // Tar bort rubrikraden innan vidare bearbetning
  listData.removeAt(0);
  
  // Konverterar till List<Map<String, dynamic>> för ett mer strukturerat format
  List<Map<String, dynamic>> mappadData = listData.map((lista) {
    Map<String, dynamic> karta = {};
    for (int i = 0; i < huvuden.length; i++) {
      karta[huvuden[i]] = lista[i];
    }
    return karta;
  }).toList();
  
  // Skriver ut den mappade datan
  print(mappadData);
}

// Exempel på utdata:
// [{Namn: John Doe, Ålder: 30, E-post: john@example.com}, {Namn: Jane Smith, Ålder: 25, E-post: jane@example.com}]
```

Båda metoderna demonstrerar hur man jobbar med CSV-data: den första manuellt, för lärande syften eller när man hanterar mycket enkla CSV-strukturer; den andra, genom att utnyttja ett kraftfullt bibliotek som förenklar tolkningen och kan hantera olika komplexiteter av CSV-formatering.
