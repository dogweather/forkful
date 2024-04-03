---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:11.074652-07:00
description: "Lavorare con JSON (JavaScript Object Notation) implica l'analisi di\
  \ dati JSON da stringhe a oggetti Dart e viceversa, un'attivit\xE0 comune nello\
  \ sviluppo\u2026"
lastmod: '2024-03-13T22:44:43.155377-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON (JavaScript Object Notation) implica l'analisi di dati\
  \ JSON da stringhe a oggetti Dart e viceversa, un'attivit\xE0 comune nello sviluppo\
  \ web e di app per lo scambio di dati."
title: Lavorare con JSON
weight: 38
---

## Come fare:
Dart offre supporto integrato per JSON con la libreria `dart:convert`, rendendo semplice codificare e decodificare JSON. Di seguito sono riportati esempi che mostrano le operazioni di base:

**Analisi di Stringa JSON in Oggetto Dart:**
```dart
import 'dart:convert';

void main() {
  // Stringa JSON di esempio
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Decodifica di JSON in Mappa Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Ciao, ${user['name']}! Hai ${user['age']} anni.');
  // Output: Ciao, John! Hai 30 anni.
}
```

**Codifica di Oggetto Dart in Stringa JSON:**
```dart
import 'dart:convert';

void main() {
  // Oggetto Dart di esempio
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Codifica di Mappa Dart in JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Output: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Uso di `json_serializable` per Modelli Complessi:**
Per modelli di dati complessi, la serializzazione manuale può essere onerosa. Il pacchetto `json_serializable` automatizza questo processo. Richiede una configurazione aggiuntiva, inclusa l'aggiunta di dipendenze nel proprio `pubspec.yaml` e la creazione di file di compilazione. Dopo la configurazione, è possibile usarlo come segue:

1. Definire un modello con annotazioni:
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

2. Generare la struttura di serializzazione:
Utilizzare il comando build runner per generare il file `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Usare il proprio modello:
```dart
void main() {
  // Analisi di JSON in Utente
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Utente: ${user.name}, Età: ${user.age}');
  // Output: Utente: John, Età: 30

  // Conversione dell'Utente di nuovo in JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Output: {"name":"John","age":30,"email":"john@example.com"}
}
```

Questi esempi illustrano le interazioni di base e avanzate con JSON in Dart, permettendo agli sviluppatori di gestire compiti di serializzazione dei dati nelle loro applicazioni senza problemi.
