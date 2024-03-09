---
title:                "Работа с JSON"
date:                  2024-03-08T21:57:54.316419-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Работа с JSON (JavaScript Object Notation) включает в себя преобразование данных JSON из строк в объекты Dart и наоборот, что является обычной задачей в разработке веб-сайтов и приложений для обмена данными. Программисты делают это для эффективной обработки данных из API, конфигураций или для межкомпонентного общения внутри своих приложений.

## Как это сделать:

Dart предоставляет встроенную поддержку для JSON с библиотекой `dart:convert`, что упрощает кодирование и декодирование JSON. Ниже приведены примеры, демонстрирующие основные операции:

**Преобразование строки JSON в объект Dart:**
```dart
import 'dart:convert';

void main() {
  // Пример строки JSON
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // Декодирование JSON в карту Dart
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Привет, ${user['name']}! Тебе ${user['age']} лет.');
  // Вывод: Привет, John! Тебе 30 лет.
}
```

**Кодирование объекта Dart в строку JSON:**
```dart
import 'dart:convert';

void main() {
  // Пример объекта Dart
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Кодирование карты Dart в JSON
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Вывод: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**Использование `json_serializable` для сложных моделей:**
Для сложных моделей данных ручная сериализация может быть обременительной. Пакет `json_serializable` автоматизирует этот процесс. Он требует дополнительной настройки, включая добавление зависимостей в ваш `pubspec.yaml` и создание файлов сборки. После настройки его можно использовать следующим образом:

1. Определите модель с аннотациями:
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

2. Сгенерируйте вспомогательный код сериализации:
Используйте команду сборщика для создания файла `user.g.dart`:
```shell
flutter pub run build_runner build
```

3. Используйте вашу модель:
```dart
void main() {
  // Преобразование JSON в User
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Пользователь: ${user.name}, Возраст: ${user.age}');
  // Вывод: Пользователь: John, Возраст: 30

  // Преобразование User обратно в JSON
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Вывод: {"name":"John","age":30,"email":"john@example.com"}
}
```

Эти примеры иллюстрируют базовое и продвинутое взаимодействие с JSON в Dart, давая разработчикам возможность без труда выполнять задачи по сериализации данных в своих приложениях.
