---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:01.687998-07:00
description: "Hur man g\xF6r: Dart erbjuder inbyggt st\xF6d f\xF6r JSON med biblioteket\
  \ `dart:convert`, vilket g\xF6r det enkelt att koda och avkoda JSON. Nedan f\xF6\
  ljer exempel som\u2026"
lastmod: '2024-03-13T22:44:37.635368-06:00'
model: gpt-4-0125-preview
summary: "Dart erbjuder inbyggt st\xF6d f\xF6r JSON med biblioteket `dart:convert`,\
  \ vilket g\xF6r det enkelt att koda och avkoda JSON."
title: Att Arbeta med JSON
weight: 38
---

## Hur man gör:
Dart erbjuder inbyggt stöd för JSON med biblioteket `dart:convert`, vilket gör det enkelt att koda och avkoda JSON. Nedan följer exempel som visar grundläggande operationer:

**Tolka JSON-sträng till Dart-objekt:**
```dart
import 'dart:convert';

void main() {
  // Exempel på JSON-sträng
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Avkodar JSON till Dart Map
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hej, ${user['name']}! Du är ${user['age']} år gammal.');
  // Utskrift: Hej, John! Du är 30 år gammal.
}
```

**Koda Dart-objekt till JSON-sträng:**
```dart
import 'dart:convert';

void main() {
  // Exempel på Dart-objekt
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Kodar Dart Map till JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Utskrift: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Använda `json_serializable` för Komplexa Modeller:**
För komplexa datamodeller kan manuell serialisering vara besvärlig. Paketet `json_serializable` automatiserar denna process. Det kräver ytterligare installation, inklusive att lägga till beroenden i din `pubspec.yaml` och att skapa byggfiler. Efter installationen kan du använda den enligt följande:

1. Definiera en modell med annotationer:
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

2. Generera boilerplate för serialisering:
Använd build runner-kommandot för att generera filen `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Använd din modell:
```dart
void main() {
  // Tolka JSON till User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Användare: ${user.name}, Ålder: ${user.age}');
  // Utskrift: Användare: John, Ålder: 30

  // Konvertera User tillbaka till JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Utskrift: {"name":"John","age":30,"email":"john@example.com"}
}
```

Dessa exempel illustrerar grundläggande och avancerade JSON-interaktioner i Dart, vilket ger utvecklare möjlighet att sömlöst hantera uppgifter för data-serialisering i sina applikationer.
