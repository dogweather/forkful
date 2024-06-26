---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:39.523964-07:00
description: "Hoe te: Dart biedt een eenvoudige benadering om toegang te krijgen tot\
  \ commandoregelargumenten via de `List<String> args` in de hoofdmethode. Hieronder\u2026"
lastmod: '2024-03-13T22:44:50.521932-06:00'
model: gpt-4-0125-preview
summary: Dart biedt een eenvoudige benadering om toegang te krijgen tot commandoregelargumenten
  via de `List<String> args` in de hoofdmethode.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe te:
Dart biedt een eenvoudige benadering om toegang te krijgen tot commandoregelargumenten via de `List<String> args` in de hoofdmethode. Hieronder staat een eenvoudig voorbeeld dat demonstreert hoe je commandoregelargumenten kunt lezen en gebruiken.

```dart
// main.dart
void main(List<String> args) {
  print('Command Line Arguments:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Om dit Dart-programma uit te voeren en commandoregelargumenten door te geven, gebruik je de Dart CLI als volgt:

```shell
dart run main.dart Hallo Wereld!
```

Verwachte uitvoer:

```
Command Line Arguments:
1: Hallo
2: Wereld!
```

### Een Populaire Bibliotheek van Derden Gebruiken: `args`
Hoewel de ingebouwde mogelijkheden van Dart voor het afhandelen van commandoregelargumenten robuust zijn voor veel toepassingen, biedt het `args`-pakket een verfijnde manier om commandoregelargumenten te definiëren en te analyseren voor complexere behoeften.

Voeg eerst het `args`-pakket toe aan je `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Gebruik het vervolgens in je programma als volgt:

```dart
// Met het 'args'-pakket
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hallo, ${argResults['name']}!');
  } else {
    print('Geen naam opgegeven.');
  }
}
```

Voer het programma uit met een benoemd argument:

```shell
dart run main.dart --name=John
```

Verwachte uitvoer:

```
Hallo, John!
```

Deze eenvoudige introductie tot het analyseren van commandoregelargumenten, zowel native als met de `args`-bibliotheek, laat zien hoe Dart gebruikersinvoer direct vanuit de console kan afhandelen, en opent een pad naar het creëren van meer interactieve en dynamische CLI-applicaties.
