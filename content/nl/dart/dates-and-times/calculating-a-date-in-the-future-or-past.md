---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:16.642557-07:00
description: "Hoe: Dart biedt robuuste ondersteuning voor datummanipulatie door middel\
  \ van zijn `DateTime` klasse. Hier is hoe je toekomstige of verleden data kunt\u2026"
lastmod: '2024-03-13T22:44:50.519432-06:00'
model: gpt-4-0125-preview
summary: Dart biedt robuuste ondersteuning voor datummanipulatie door middel van zijn
  `DateTime` klasse.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe:
Dart biedt robuuste ondersteuning voor datummanipulatie door middel van zijn `DateTime` klasse. Hier is hoe je toekomstige of verleden data kunt berekenen met gebruik van enkel Dart, zonder de noodzaak van externe bibliotheken.

### Een Toekomstige Datum Berekenen
Om een datum in de toekomst te berekenen, maak je een `DateTime` object en gebruik je de `add` methode met de gewenste duur.

```dart
DateTime vandaag = DateTime.now();
Duration tienDagen = Duration(days: 10);
DateTime toekomstigeDatum = vandaag.add(tienDagen);

print(toekomstigeDatum); // Uitvoer: 2023-04-21 14:22:35.123456 (voorbeelduitvoer, afhankelijk van huidige datum en tijd)
```

### Een Verleden Datum Berekenen
Om een datum in het verleden te berekenen, gebruik je de `subtract` methode op een `DateTime` object met de benodigde duur.

```dart
DateTime vandaag = DateTime.now();
Duration vijftienDagenGeleden = Duration(days: 15);
DateTime verledenDatum = vandaag.subtract(vijftienDagenGeleden);

print(verledenDatum); // Uitvoer: 2023-03-27 14:22:35.123456 (voorbeelduitvoer, afhankelijk van huidige datum en tijd)
```

### Gebruik van Externe Bibliotheken
Hoewel Dart's ingebouwde mogelijkheden voor datummanipulatie krachtig zijn, vind je misschien dat je meer specifieke operaties nodig hebt, zoals het gemakkelijker parseren of formatteren van data, of het uitvoeren van complexe berekeningen. In zulke gevallen kan het `time` pakket zeer nuttig zijn.

Voeg eerst `time` toe aan je `pubspec.yaml` afhankelijkheden:

```yaml
dependencies:
  time: ^2.0.0
```

Vervolgens kun je het gebruiken om vergelijkbare berekeningen uit te voeren met verbeterde leesbaarheid:

```dart
import 'package:time/time.dart';

void main() {
  DateTime vandaag = DateTime.now();

  // Een toekomstige datum berekenen
  DateTime toekomstigeDatum = vandaag + 10.days;
  print(toekomstigeDatum); // Uitvoerformaat: 2023-04-21 14:22:35.123456

  // Een verleden datum berekenen
  DateTime verledenDatum = vandaag - 15.days;
  print(verledenDatum); // Uitvoerformaat: 2023-03-27 14:22:35.123456
}
```

Deze voorbeelden illustreren basisdatummanipulaties in Dart, inclusief het toevoegen en aftrekken van tijd aan of van een huidige datum, en demonstreren hoe moeiteloos data beheerd kunnen worden in Dart-applicaties.
