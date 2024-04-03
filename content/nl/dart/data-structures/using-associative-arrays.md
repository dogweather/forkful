---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:11.297878-07:00
description: "Hoe te: Dart biedt een eenvoudige syntaxis voor het cre\xEBren en manipuleren\
  \ van Maps. Hieronder zijn voorbeelden die basisbewerkingen demonstreren zoals\u2026"
lastmod: '2024-03-13T22:44:50.497217-06:00'
model: gpt-4-0125-preview
summary: "Dart biedt een eenvoudige syntaxis voor het cre\xEBren en manipuleren van\
  \ Maps."
title: Gebruik makend van associatieve arrays
weight: 15
---

## Hoe te:
Dart biedt een eenvoudige syntaxis voor het creëren en manipuleren van Maps. Hieronder zijn voorbeelden die basisbewerkingen demonstreren zoals het creëren, toevoegen van elementen en het ophalen van waarden.

```dart
void main() {
  // Een map creëren
  var fruitKleuren = {
    'appel': 'rood',
    'banaan': 'geel',
    'druif': 'paars'
  };

  // Een nieuw sleutel-waardepaar toevoegen
  fruitKleuren['sinaasappel'] = 'oranje';

  // Een waarde opvragen via zijn sleutel
  print(fruitKleuren['appel']); // Uitvoer: rood

  // Een waarde bijwerken
  fruitKleuren['banaan'] = 'groen';

  // Itereren over de Map
  fruitKleuren.forEach((fruit, kleur) {
    print('$fruit: $kleur');
  });
  // Voorbeelduitvoer:
  // appel: rood
  // banaan: groen
  // druif: paars
  // sinaasappel: oranje
}
```

Voor complexe datastructuren of uitgebreide functionaliteit vertrouwen Dart-programmeurs vaak op aanvullende bibliotheken. Een dergelijke bibliotheek is `collection`, die geavanceerde collectietypes en hulpprogramma's biedt. Hoewel `collection` de basismanier waarop Maps worden behandeld niet wijzigt, verrijkt het deze met hulpprogrammafuncties en meer verfijnde collectietypes. Hier is hoe je het zou kunnen gebruiken voor een specifiekere taak, zoals het sorteren van een Map op basis van zijn waarden:

Zorg eerst dat het `collection` pakket is opgenomen in je `pubspec.yaml` bestand:

```yaml
dependencies:
  collection: ^1.15.0
```

Vervolgens kun je het als volgt gebruiken:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitKleuren = {
    'appel': 'rood',
    'banaan': 'geel',
    'druif': 'paars',
    'sinaasappel': 'oranje'
  };

  // De Map sorteren op basis van zijn waarden (kleuren)
  var gesorteerdeFruitOpKleur = SplayTreeMap.from(
    fruitKleuren,
    (sleutel1, sleutel2) => fruitKleuren[sleutel1]!.compareTo(fruitKleuren[sleutel2]!)
  );

  print(gesorteerdeFruitOpKleur);
  // Uitvoer:
  // {sinaasappel: oranje, appel: rood, banaan: geel, druif: paars}
}
```

Dit voorbeeld demonstreert het sorteren van de entries van een Map op basis van hun waarden, en toont aan hoe Dart en zijn levendige ecosysteem associatieve arrays behendig kunnen hanteren voor meer verfijnde gegevensmanipulatie.
