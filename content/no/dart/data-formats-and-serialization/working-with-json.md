---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:20.760290-07:00
description: "\xC5 jobbe med JSON (JavaScript Object Notation) inneb\xE6rer \xE5 analysere\
  \ JSON-data fra strenger til Dart-objekter og omvendt, en vanlig oppgave i web-\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.511091-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON (JavaScript Object Notation) inneb\xE6rer \xE5 analysere\
  \ JSON-data fra strenger til Dart-objekter og omvendt, en vanlig oppgave i web-\
  \ og\u2026"
title: Arbeide med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med JSON (JavaScript Object Notation) innebærer å analysere JSON-data fra strenger til Dart-objekter og omvendt, en vanlig oppgave i web- og apputvikling for datautveksling. Programmerere gjør det for effektivt å håndtere data fra API-er, konfigurasjoner eller interkomponentkommunikasjon innenfor deres apper.

## Hvordan:

Dart gir innebygd støtte for JSON med `dart:convert`-biblioteket, noe som gjør det enkelt å kode og dekode JSON. Nedenfor er eksempler som viser grunnleggende operasjoner:

**Analysering av JSON-streng til Dart-objekt:**
```dart
import 'dart:convert';

void main() {
  // Eksempel på JSON-streng
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Dekoding av JSON til Dart Map
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hallo, ${user['name']}! Du er ${user['age']} år gammel.');
  // Utdata: Hallo, John! Du er 30 år gammel.
}
```

**Koding av Dart-objekt til JSON-streng:**
```dart
import 'dart:convert';

void main() {
  // Eksempel på Dart-objekt
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Koding av Dart Map til JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Utdata: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Bruk av `json_serializable` for Komplekse Modeller:**
For komplekse datamodeller kan manuell serialisering være tungvint. Pakken `json_serializable` automatiserer denne prosessen. Den krever ekstra oppsett, inkludert å legge til avhengigheter i din `pubspec.yaml` og å lage build-filer. Etter oppsettet kan du bruke den som følger:

1. Definer en modell med annotasjoner:
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  fabrikk User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. Generer serialiseringens malverk:
Bruk build runner-kommandoen for å generere `user.g.dart`-filen:
```shell
flutter pub run build_runner build
```

3. Bruk modellen din:
```dart
void main() {
  // Analysering av JSON til User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Bruker: ${user.name}, Alder: ${user.age}');
  // Utdata: Bruker: John, Alder: 30

  // Konvertere User tilbake til JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Utdata: {"name":"John","age":30,"email":"john@example.com"}
}
```

Disse eksemplene illustrerer grunnleggende og avanserte JSON-interaksjoner i Dart, noe som gir utviklere muligheten til sømløst å håndtere oppgaver med dataserialisering i sine applikasjoner.
