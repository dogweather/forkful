---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:29.538316-07:00
description: "Praca z JSON (JavaScript Object Notation) obejmuje parsowanie danych\
  \ JSON z ci\u0105g\xF3w znak\xF3w na obiekty Dart i odwrotnie, co jest powszechnym\
  \ zadaniem w\u2026"
lastmod: '2024-03-13T22:44:35.116177-06:00'
model: gpt-4-0125-preview
summary: "Praca z JSON (JavaScript Object Notation) obejmuje parsowanie danych JSON\
  \ z ci\u0105g\xF3w znak\xF3w na obiekty Dart i odwrotnie, co jest powszechnym zadaniem\
  \ w rozwoju stron internetowych i aplikacji mobilnych dla wymiany danych."
title: Praca z JSON
weight: 38
---

## Co i dlaczego?

Praca z JSON (JavaScript Object Notation) obejmuje parsowanie danych JSON z ciągów znaków na obiekty Dart i odwrotnie, co jest powszechnym zadaniem w rozwoju stron internetowych i aplikacji mobilnych dla wymiany danych. Programiści wykonują to, aby efektywnie obsługiwać dane z API, konfiguracji lub komunikacji między komponentami w ich aplikacjach.

## Jak to zrobić:

Dart oferuje wbudowane wsparcie dla JSON za pomocą biblioteki `dart:convert`, co ułatwia kodowanie i dekodowanie JSON. Poniżej znajdują się przykłady przedstawiające podstawowe operacje:

**Parsowanie ciągu JSON na obiekt Dart:**
```dart
import 'dart:convert';

void main() {
  // Przykładowy ciąg JSON
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Dekodowanie JSON do Mapy Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Witaj, ${user['name']}! Masz ${user['age']} lat.');
  // Wynik: Witaj, John! Masz 30 lat.
}
```

**Kodowanie obiektu Dart na ciąg JSON:**
```dart
import 'dart:convert';

void main() {
  // Przykładowy obiekt Dart
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Kodowanie Mapy Dart do JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Wynik: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Używanie `json_serializable` dla skomplikowanych modeli:**
Dla skomplikowanych modeli danych, ręczna serializacja może być uciążliwa. Pakiet `json_serializable` automatyzuje ten proces. Wymaga dodatkowej konfiguracji, w tym dodania zależności do pliku `pubspec.yaml` oraz tworzenia plików budowania. Po konfiguracji, można go używać w następujący sposób:

1. Zdefiniuj model z adnotacjami:
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

2. Wygeneruj boilerplate do serializacji:
Użyj polecenia build runner, aby wygenerować plik `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Użyj swojego modelu:
```dart
void main() {
  // Parsowanie JSON na User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Użytkownik: ${user.name}, Wiek: ${user.age}');
  // Wynik: Użytkownik: John, Wiek: 30

  // Konwersja User z powrotem na JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Wynik: {"name":"John","age":30,"email":"john@example.com"}
}
```

Te przykłady ilustrują podstawowe i zaawansowane interakcje z JSON w Dart, umożliwiając programistom płynne obsługiwanie zadań serializacji danych w ich aplikacjach.
