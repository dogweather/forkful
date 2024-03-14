---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:44.939337-07:00
description: "Die Arbeit mit JSON (JavaScript Object Notation) beinhaltet das Parsen\
  \ von JSON-Daten aus Strings in Dart-Objekte und umgekehrt, eine g\xE4ngige Aufgabe\
  \ in\u2026"
lastmod: '2024-03-13T22:44:53.604507-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit JSON (JavaScript Object Notation) beinhaltet das Parsen von\
  \ JSON-Daten aus Strings in Dart-Objekte und umgekehrt, eine g\xE4ngige Aufgabe\
  \ in\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit JSON (JavaScript Object Notation) beinhaltet das Parsen von JSON-Daten aus Strings in Dart-Objekte und umgekehrt, eine gängige Aufgabe in der Web- und App-Entwicklung für den Datenaustausch. Programmierer tun dies, um Daten von APIs, Konfigurationen oder der internen Komponentenkommunikation innerhalb ihrer Apps effizient zu verarbeiten.

## Wie geht das:

Dart bietet mit der Bibliothek `dart:convert` eingebaute Unterstützung für JSON, was das Codieren und Dekodieren von JSON sehr einfach macht. Unten finden Sie Beispiele, die grundlegende Operationen zeigen:

**JSON-String in Dart-Objekt parsen:**
```dart
import 'dart:convert';

void main() {
  // Beispiel JSON-String
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // JSON zu Dart Map dekodieren
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hallo, ${user['name']}! Du bist ${user['age']} Jahre alt.');
  // Ausgabe: Hallo, John! Du bist 30 Jahre alt.
}
```

**Dart-Objekt in JSON-String kodieren:**
```dart
import 'dart:convert';

void main() {
  // Beispiel Dart-Objekt
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Dart Map in JSON kodieren
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Ausgabe: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Verwendung von `json_serializable` für komplexe Modelle:**
Für komplexe Datenmodelle kann die manuelle Serialisierung mühsam sein. Das Paket `json_serializable` automatisiert diesen Prozess. Es erfordert zusätzliche Einrichtungen, einschließlich des Hinzufügens von Abhängigkeiten zu Ihrer `pubspec.yaml` und dem Erstellen von Build-Dateien. Nach der Einrichtung können Sie es wie folgt verwenden:

1. Ein Modell mit Annotationen definieren:
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

2. Das Serialisierungs-Gerüst generieren:
Verwenden Sie den Build Runner-Befehl, um die Datei `user.g.dart` zu generieren:
```shell
flutter pub run build_runner build
```

3. Ihr Modell verwenden:
```dart
void main() {
  // JSON in User parsen
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Benutzer: ${user.name}, Alter: ${user.age}');
  // Ausgabe: Benutzer: John, Alter: 30

  // User zurück in JSON konvertieren
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Ausgabe: {"name":"John","age":30,"email":"john@example.com"}
}
```

Diese Beispiele illustrieren grundlegende und fortgeschrittene JSON-Interaktionen in Dart und ermächtigen Entwickler dazu, Aufgaben zur Daten-Serialisierung in ihren Anwendungen nahtlos zu handhaben.
