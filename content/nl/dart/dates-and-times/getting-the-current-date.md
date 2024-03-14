---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.366236-07:00
description: "De huidige datum in Dart verkrijgen houdt in dat het systeem wordt geraadpleegd\
  \ voor de huidige datum en tijd. Deze functionaliteit wordt vaak gebruikt in\u2026"
lastmod: '2024-03-13T22:44:50.516319-06:00'
model: gpt-4-0125-preview
summary: "De huidige datum in Dart verkrijgen houdt in dat het systeem wordt geraadpleegd\
  \ voor de huidige datum en tijd. Deze functionaliteit wordt vaak gebruikt in\u2026"
title: De huidige datum krijgen
---

{{< edit_this_page >}}

## Wat & Waarom?
De huidige datum in Dart verkrijgen houdt in dat het systeem wordt geraadpleegd voor de huidige datum en tijd. Deze functionaliteit wordt vaak gebruikt in applicaties voor functies zoals het tijdstempelen van gebeurtenissen, het tonen van de huidige datum aan gebruikers of het berekenen van duur. Weten hoe je efficiënt de huidige datum kunt ophalen en manipuleren is fundamenteel voor planning, loggen en tijdgevoelige functies.

## Hoe:
De kernbibliotheek van Dart biedt eenvoudige toegang tot de huidige datum en tijd via de `DateTime` klasse. Hier is het basisvoorbeeld om de huidige datum te krijgen:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Voorbeelduitvoer: 2023-04-12 10:00:00.000
}
```

Als je alleen het deel van de datum (jaar, maand, dag) nodig hebt, kun je het `DateTime` object formatteren:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Voorbeelduitvoer: 2023-04-12
}
```

Dart bevat geen ingebouwde bibliotheek voor meer complexe datumformaten, maar je kunt het `intl` pakket gebruiken voor dit doel. Voeg eerst het pakket toe aan je `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Daarna kun je datums gemakkelijk formatteren:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Voorbeelduitvoer: 2023-04-12
}
```

Voor meer geavanceerde formateringsopties, verken de `DateFormat` klasse die wordt aangeboden door het `intl` pakket, welke een breed scala aan patronen en locaties ondersteunt.
