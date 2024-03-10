---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:24.133504-07:00
description: "Travailler avec des fichiers CSV (Comma Separated Values), c'est-\xE0\
  -dire des fichiers texte o\xF9 chaque ligne contient des valeurs s\xE9par\xE9es\
  \ par des virgules,\u2026"
lastmod: '2024-03-09T21:06:21.262491-07:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Comma Separated Values), c'est-\xE0-dire\
  \ des fichiers texte o\xF9 chaque ligne contient des valeurs s\xE9par\xE9es par\
  \ des virgules,\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Comma Separated Values), c'est-à-dire des fichiers texte où chaque ligne contient des valeurs séparées par des virgules, permet aux programmeurs de faciliter l'échange de données entre différentes applications ou de stocker des données dans un format léger et lisible par l'homme.

## Comment faire :

Pour gérer les fichiers CSV en Dart, vous pouvez soit traiter le texte manuellement, soit utiliser des bibliothèques tierces pour simplifier la tâche. Ici, nous examinerons les deux approches.

### Analyse manuelle de CSV

Si vos besoins sont simples, vous pourriez choisir d'analyser manuellement une chaîne CSV. Cela peut être réalisé en utilisant les fonctions de manipulation de chaînes de caractères de base de Dart :

```dart
void main() {
  // Données CSV d'exemple
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Division des données CSV en lignes
  List<String> lines = csvData.split('\n');
  
  // Analyse de chaque ligne
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
  
  // Affichage des données analysées
  print(data);
}

// Exemple de sortie :
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Utilisation d'une bibliothèque tierce : `csv`

Pour des scénarios plus complexes ou pour simplifier votre code, vous pouvez utiliser une bibliothèque tierce populaire comme `csv`. Commencez par l'ajouter à votre projet en incluant `csv: ^5.0.0` (ou la version la plus récente) dans votre fichier `pubspec.yaml` sous `dependencies`. Utilisez-la ensuite comme suit :

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Utilisez CsvToListConverter pour analyser les données CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Le premier élément de la liste contient les en-têtes
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // Suppression de la ligne d'en-tête avant de traiter plus loin
  listData.removeAt(0);
  
  // Convertir en List<Map<String, dynamic>> pour un format plus structuré
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Affichage des données mappées
  print(mappedData);
}

// Exemple de sortie :
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Les deux méthodes démontrent comment travailler avec des données CSV : la première manuellement, à des fins d'apprentissage ou lors de la manipulation de structures CSV très simples ; la seconde, en utilisant une bibliothèque puissante qui simplifie l'analyse et peut gérer différentes complexités de formatage CSV.
