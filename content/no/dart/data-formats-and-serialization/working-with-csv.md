---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:26.808044-07:00
description: "Hvordan: For \xE5 h\xE5ndtere CSV-filer i Dart, behandler du vanligvis\
  \ teksten manuelt eller bruker tredjepartsbiblioteker for \xE5 forenkle oppgaven.\
  \ Her vil vi\u2026"
lastmod: '2024-03-13T22:44:40.512131-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 h\xE5ndtere CSV-filer i Dart, behandler du vanligvis teksten manuelt\
  \ eller bruker tredjepartsbiblioteker for \xE5 forenkle oppgaven."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
For å håndtere CSV-filer i Dart, behandler du vanligvis teksten manuelt eller bruker tredjepartsbiblioteker for å forenkle oppgaven. Her vil vi se på begge tilnærminger.

### Manuell Analyse av CSV
Hvis dine behov er enkle, kan du velge å manuelt analysere en CSV-streng. Dette kan oppnås ved å bruke Darts kjernefunksjoner for strengmanipulering:

```dart
void main() {
  // Eksempel på CSV-data
  String csvData = "Navn,Alder,Epost\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Splitte CSV-dataene inn i linjer
  List<String> linjer = csvData.split('\n');
  
  // Analysere hver linje
  List<Map<String, String>> data = [];
  List<String> overskrifter = linjer.first.split(',');
  
  for (var i = 1; i < linjer.length; i++) {
    List<String> rad = linjer[i].split(',');
    Map<String, String> post = {};
    for (var j = 0; j < overskrifter.length; j++) {
      post[overskrifter[j]] = rad[j];
    }
    data.add(post);
  }
  
  // Vise den analyserte dataen
  print(data);
}

// Eksempel på utskrift:
// [{Navn: John Doe, Alder: 30, Epost: john@example.com}, {Navn: Jane Smith, Alder: 25, Epost: jane@example.com}]
```

### Bruke et Tredjepartsbibliotek: `csv`
For mer komplekse scenarioer eller for å forenkle koden din, kan du bruke et populært tredjepartsbibliotek som `csv`. Først, legg det til i prosjektet ditt ved å inkludere `csv: ^5.0.0` (eller den nyeste versjonen) i din `pubspec.yaml`-fil under `dependencies`. Deretter bruker du det som følger:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Navn,Alder,Epost\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Bruk CsvToListConverter for å analysere CSV-dataene
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Det første liste-elementet inneholder overskrifter
  List<String> overskrifter = listData.first.map((item) => item.toString()).toList();
  
  // Fjerne overskriftrad før videre bearbeiding
  listData.removeAt(0);
  
  // Konverter til List<Map<String, dynamic>> for et mer strukturert format
  List<Map<String, dynamic>> kartlagteData = listData.map((liste) {
    Map<String, dynamic> kart = {};
    for (int i = 0; i < overskrifter.length; i++) {
      kart[overskrifter[i]] = liste[i];
    }
    return kart;
  }).toList();
  
  // Vise de kartlagte dataene
  print(kartlagteData);
}

// Eksempel på utskrift:
// [{Navn: John Doe, Alder: 30, Epost: john@example.com}, {Navn: Jane Smith, Alder: 25, Epost: jane@example.com}]
```

Begge metodene demonstrerer hvordan man jobber med CSV-data: den første manuelt, for læreformål eller når man har med svært enkle CSV-strukturer å gjøre; den andre, ved å bruke et kraftig bibliotek som forenkler parsingen og kan håndtere ulike kompleksiteter av CSV-formatet.
