---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:43.361993-07:00
description: "Fouten afhandelen in Dart gaat over het anticiperen op en beheren van\
  \ uitzonderingen die ontstaan tijdens de uitvoering van het programma om de\u2026"
lastmod: '2024-03-13T22:44:50.513261-06:00'
model: gpt-4-0125-preview
summary: Fouten afhandelen in Dart gaat over het anticiperen op en beheren van uitzonderingen
  die ontstaan tijdens de uitvoering van het programma om de betrouwbaarheid en bruikbaarheid
  te verbeteren.
title: Fouten afhandelen
weight: 16
---

## Wat & Waarom?
Fouten afhandelen in Dart gaat over het anticiperen op en beheren van uitzonderingen die ontstaan tijdens de uitvoering van het programma om de betrouwbaarheid en bruikbaarheid te verbeteren. Programmeurs implementeren foutafhandeling om crashes te voorkomen en zinvolle feedback aan gebruikers te bieden, wat zorgt voor een soepelere, veiligere applicatie-ervaring.

## Hoe doe je dat:
Dart ondersteunt twee soorten fouten: *compileertijd* fouten en *looptijd* fouten. Compileertijd fouten worden gedetecteerd door de Dart-analyzer voordat de code wordt uitgevoerd, terwijl looptijd fouten, of uitzonderingen, optreden tijdens de uitvoering. Hier is hoe je uitzonderingen in Dart afhandelt:

### Probeer-Vang
Gebruik `try-catch` om uitzonderingen te vangen en te voorkomen dat ze je applicatie laten crashen:

```dart
try {
  var resultaat = 100 ~/ 0; // Poging tot delen door nul, veroorzaakt een uitzondering
} catch (e) {
  print('Een uitzondering gevangen: $e'); // Handelt de uitzondering af
}
```
Voorbeelduitvoer: `Een uitzondering gevangen: IntegerDivisionByZeroException`

### Specifieke Uitzondering
Om specifieke uitzonderingen af te handelen, vermeld je de uitzondering na `catch`:

```dart
try {
  var resultaat = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Kan niet delen door nul.'); // Handelt specifiek het delen door nul uitzonderingen af
}
```
Voorbeelduitvoer: `Kan niet delen door nul.`

### Stack Trace
Om een stack trace te krijgen voor debuggen, gebruik je een tweede parameter in het catch-blok:

```dart
try {
  var resultaat = 100 ~/ 0;
} catch (e, s) {
  print('Uitzondering: $e');
  print('Stack trace: $s'); // Print stack trace voor debuggen
}
```

### Tot Slot
Gebruik `finally` om code uit te voeren na try/catch, ongeacht of er een uitzondering was:

```dart
try {
  var resultaat = 100 ~/ 0;
} catch (e) {
  print('Een uitzondering gevangen: $e');
} finally {
  print('Dit wordt altijd uitgevoerd.'); // Opruimcode of laatste stappen
}
```
Voorbeelduitvoer:
```
Een uitzondering gevangen: IntegerDivisionByZeroException
Dit wordt altijd uitgevoerd.
```

### Bibliotheken van Derden
Hoewel de kernbibliotheek van Dart robuust is voor foutafhandeling, kun je ook gebruikmaken van pakketten van derden zoals `dartz` voor functioneel programmeren die concepten introduceert zoals `Either` en `Option` die kunnen worden gebruikt voor foutafhandeling. Hier is een voorbeeld van het gebruik van `dartz` voor foutafhandeling:

1. Voeg `dartz` toe aan je `pubspec.yaml` bestand onder afhankelijkheden:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Gebruik `Either` om fouten op een elegante manier af te handelen in je Dart-code:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> delen(int dividend, int deler) {
  if (deler == 0) {
    return Left('Kan niet delen door nul.');
  } else {
    return Right(dividend ~/ deler);
  }
}

void main() {
  final resultaat = delen(100, 0);
  resultaat.fold(
    (links) => print('Fout: $links'), 
    (rechts) => print('Resultaat: $rechts')
  );
}
```
Voorbeelduitvoer: `Fout: Kan niet delen door nul.`

Het `Left` deel vertegenwoordigt meestal de fout, en het `Right` deel vertegenwoordigt succes. Dit patroon maakt het mogelijk fouten op een functionelere manier af te handelen, en biedt duidelijkheid en controle over foutbeheer.
