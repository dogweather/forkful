---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.662757-07:00
description: "Werken met JSON (JavaScript Object Notation) houdt in dat JSON-gegevens\
  \ worden omgezet van strings naar Dart-objecten en vice versa, een veelvoorkomende\u2026"
lastmod: '2024-03-13T22:44:50.528007-06:00'
model: gpt-4-0125-preview
summary: "Werken met JSON (JavaScript Object Notation) houdt in dat JSON-gegevens\
  \ worden omgezet van strings naar Dart-objecten en vice versa, een veelvoorkomende\u2026"
title: Werken met JSON
weight: 38
---

## Wat & Waarom?

Werken met JSON (JavaScript Object Notation) houdt in dat JSON-gegevens worden omgezet van strings naar Dart-objecten en vice versa, een veelvoorkomende taak bij web- en app-ontwikkeling voor gegevensuitwisseling. Programmeurs doen dit om op een efficiënte manier om te gaan met gegevens van API's, configuraties of communicatie tussen onderdelen binnen hun apps.

## Hoe te:

Dart biedt ingebouwde ondersteuning voor JSON met de `dart:convert`-bibliotheek, wat het coderen en decoderen van JSON eenvoudig maakt. Hieronder zijn voorbeelden die basisbewerkingen laten zien:

**JSON-string parsen naar Dart-object:**
```dart
import 'dart:convert';

void main() {
  // Voorbeeld JSON-string
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Decoding JSON naar Dart Map
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hallo, ${user['name']}! Je bent ${user['age']} jaar oud.');
  // Uitvoer: Hallo, John! Je bent 30 jaar oud.
}
```

**Dart-object coderen naar JSON-string:**
```dart
import 'dart:convert';

void main() {
  // Voorbeeld Dart-object
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Dart Map coderen naar JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Uitvoer: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Gebruik van `json_serializable` voor complexe modellen:**
Voor complexe datamodellen kan handmatige serialisatie omslachtig zijn. Het `json_serializable`-pakket automatiseert dit proces. Het vereist extra opzet, inclusief het toevoegen van afhankelijkheden aan je `pubspec.yaml` en het maken van build-bestanden. Na de opzet kun je het als volgt gebruiken:

1. Definieer een model met annotaties:
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. Genereer de serialisatie-boilerplate:
Gebruik het build runner commando om het `user.g.dart`-bestand te genereren:
```shell
flutter pub run build_runner build
```

3. Gebruik je model:
```dart
void main() {
  // JSON parsen naar Gebruiker
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Gebruiker: ${user.name}, Leeftijd: ${user.age}');
  // Uitvoer: Gebruiker: John, Leeftijd: 30

  // Gebruiker weer naar JSON converteren
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Uitvoer: {"name":"John","age":30,"email":"john@example.com"}
}
```

Deze voorbeelden illustreren de basis- en geavanceerde JSON-interacties in Dart, waardoor ontwikkelaars moeiteloos taken voor gegevensserialisatie in hun applicaties kunnen afhandelen.
