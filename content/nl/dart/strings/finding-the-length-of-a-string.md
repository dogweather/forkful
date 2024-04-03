---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:27.807210-07:00
description: 'Hoe: Dart maakt het eenvoudig om de lengte van een string te krijgen
  met behulp van de `length` eigenschap. Hier is een basisvoorbeeld.'
lastmod: '2024-03-13T22:44:50.495240-06:00'
model: gpt-4-0125-preview
summary: Dart maakt het eenvoudig om de lengte van een string te krijgen met behulp
  van de `length` eigenschap.
title: De lengte van een string vinden
weight: 7
---

## Hoe:
Dart maakt het eenvoudig om de lengte van een string te krijgen met behulp van de `length` eigenschap. Hier is een basisvoorbeeld:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("De lengte van '\(myString)' is: \(myString.length)");
  // Uitvoer: De lengte van 'Hello, Dart!' is: 12
}
```
Deze eigenschap telt het aantal UTF-16 code-eenheden in de string, wat overeenkomt met de lengte van de string voor de meeste gangbare gebruikstoepassingen.

Voor meer genuanceerde tekstverwerking, vooral met betrekking tot Unicode karakters buiten het Basic Multilingual Plane (BMP), overweeg het gebruik van het `characters` pakket voor het tellen van grafeemclusters, wat een nauwkeuriger representatie geeft van gebruikersgepercipieerde karakters.

Voeg eerst `characters` toe aan je `pubspec.yaml`:

```yaml
dependencies:
  characters: ^1.2.0
```

Gebruik het vervolgens als volgt:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 familie";
  print("De lengte van '\(myEmojiString)' is: \(myEmojiString.characters.length)");
  // Uitvoer: De lengte van '👨‍👩‍👧‍👦 familie' is: 8
}
```

In dit voorbeeld geeft `myEmojiString.characters.length` ons de lengte in termen van Unicode grafeemclusters, wat een nauwkeurigere representatie is voor strings die complexe karakters bevatten, zoals emoji's of gecombineerde karaktertekens.
