---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:37.203456-07:00
description: "Het omzetten van een datum naar een string in Dart is een veelvoorkomende\
  \ taak wanneer u datum- en tijdinformatie op een voor mensen leesbare manier wilt\u2026"
lastmod: '2024-03-09T21:06:14.699553-07:00'
model: gpt-4-0125-preview
summary: "Het omzetten van een datum naar een string in Dart is een veelvoorkomende\
  \ taak wanneer u datum- en tijdinformatie op een voor mensen leesbare manier wilt\u2026"
title: Een datum omzetten naar een string
---

{{< edit_this_page >}}

## Wat & Waarom?

Het omzetten van een datum naar een string in Dart is een veelvoorkomende taak wanneer u datum- en tijdinformatie op een voor mensen leesbare manier wilt weergeven, of wanneer u gegevens wilt serialiseren voor opslag of verzending. Dit proces maakt de eenvoudige weergave en manipulatie van datum-tijd waarden mogelijk in een formaat dat zowel begrijpelijk is als aangepast kan worden afhankelijk van het gebruik.

## Hoe te:

Dart biedt de `DateTime` klasse voor het hanteren van datums en tijden, en het `intl` pakket voor het formatteren. Zorg eerst dat u het `intl` pakket hebt door `intl: ^0.17.0` (of de nieuwste versie) toe te voegen aan uw `pubspec.yaml` bestand.

### Gebruikmakend van Dart's Kernbibliotheek

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Uitvoer: 2023-4-12 (bijvoorbeeld, dit hangt af van de huidige datum)
```

Dit voorbeeld construeert direct een string uit de eigenschappen van `DateTime`.

### Gebruikmakend van het `intl` pakket

Importeer eerst het pakket:

```dart
import 'package:intl/intl.dart';
```

Formatteer dan de datum:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Uitvoer: 2023-04-12
```

Het `intl` pakket maakt veel complexere formaten gemakkelijk mogelijk, inclusief locatiespecifieke formaten:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Uitvoer: 12 april 2023
```

Deze voorbeelden tonen eenvoudige maar krachtige manieren om datums om te zetten en te formatteren naar strings in Dart, hetzij met behulp van Dart's kernfunctionaliteit of door het `intl` pakket te gebruiken voor geavanceerdere formatteringsopties.
