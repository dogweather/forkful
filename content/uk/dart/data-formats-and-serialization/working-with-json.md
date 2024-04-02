---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:25.670956-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\
  \u0437 \u0434\u0430\u043D\u0438\u0445 JSON \u0437 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0443 \u043E\u0431'\u0454\u043A\u0442\u0438 Dart \u0456 \u043D\u0430\u0432\
  \u043F\u0430\u043A\u0438, \u0449\u043E \u0454 \u043F\u043E\u0448\u0438\u0440\u0435\
  \u043D\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\u043C \u0443\
  \ \u0432\u0435\u0431-\u0440\u043E\u0437\u0440\u043E\u0431\u0446\u0456 \u0442\u0430\
  \u2026"
lastmod: '2024-03-13T22:44:48.836372-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437\
  \ \u0434\u0430\u043D\u0438\u0445 JSON \u0437 \u0440\u044F\u0434\u043A\u0456\u0432\
  \ \u0443 \u043E\u0431'\u0454\u043A\u0442\u0438 Dart \u0456 \u043D\u0430\u0432\u043F\
  \u0430\u043A\u0438, \u0449\u043E \u0454 \u043F\u043E\u0448\u0438\u0440\u0435\u043D\
  \u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\u043C \u0443 \u0432\
  \u0435\u0431-\u0440\u043E\u0437\u0440\u043E\u0431\u0446\u0456 \u0442\u0430\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Що і чому?

Робота з JSON (JavaScript Object Notation) включає аналіз даних JSON з рядків у об'єкти Dart і навпаки, що є поширеним завданням у веб-розробці та розробці додатків для обміну даними. Програмісти роблять це для ефективного управління даними від API, конфігурацій або для комунікації між компонентами в межах їхніх додатків.

## Як:

Dart має вбудовану підтримку для JSON з бібліотекою `dart:convert`, що робить кодування та декодування JSON простим. Нижче наведено приклади, які демонструють основні операції:

**Аналіз рядка JSON до об'єкта Dart:**
```dart
import 'dart:convert';

void main() {
  // Приклад рядка JSON
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Декодування JSON до Map Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Привіт, ${user['name']}! Тобі ${user['age']} років.');
  // Вивід: Привіт, John! Тобі 30 років.
}
```

**Кодування об'єкта Dart до рядка JSON:**
```dart
import 'dart:convert';

void main() {
  // Приклад об'єкта Dart
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Кодування Map Dart до JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Вивід: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Використання `json_serializable` для складних моделей:**
Для складних моделей даних ручна серіалізація може бути обтяжливою. Пакет `json_serializable` автоматизує цей процес. Він вимагає додаткового налаштування, включно з додаванням залежностей у ваш `pubspec.yaml` і створенням файлів збірки. Після налаштування ви можете використовувати його так:

1. Визначте модель з анотаціями:
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

2. Згенеруйте шаблон серіалізації:
Використовуйте команду build runner для генерації файлу `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Використовуйте вашу модель:
```dart
void main() {
  // Аналіз JSON у User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Користувач: ${user.name}, Вік: ${user.age}');
  // Вивід: Користувач: John, Вік: 30

  // Перетворення User назад у JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Вивід: {"name":"John","age":30,"email":"john@example.com"}
}
```

Ці приклади ілюструють базові та складні взаємодії з JSON у Dart, надаючи розробникам можливість безперешкодно управляти задачами серіалізації даних у їхніх додатках.
