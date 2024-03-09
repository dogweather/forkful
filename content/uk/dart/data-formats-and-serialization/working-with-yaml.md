---
title:                "Робота з YAML"
date:                  2024-03-08T21:57:36.284181-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що і чому?

YAML, абревіатура від "YAML Ain't Markup Language", є форматом серіалізації даних, призначеним для зручного читання людиною. Програмісти використовують його для файлів налаштувань, обміну даними та в додатках, де дані потрібно зберігати або передавати у форматі, який легко зрозуміти.

## Як це зробити:

У Dart робота з YAML, як правило, передбачає використання сторонньої бібліотеки, оскільки мова не включає вбудованих можливостей аналізу YAML. Популярний вибір — пакет `yaml`. Для початку, вам потрібно додати цей пакет до вашого `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Не забудьте запустити `pub get`, щоб завантажити пакет.

### Читання YAML

Щоб прочитати файл YAML, спочатку імпортуйте пакет `yaml`, а потім використовуйте функцію `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Вивід: John Doe
}

```

Виходячи з того, що ваш файл `config.yaml` виглядає ось так:

```yaml
name: John Doe
age: 30
```

### Запис YAML

Хоча пакет `yaml` чудово підходить для аналізу, він не підтримує запис YAML. Для цього вам, можливо, доведеться вручну конвертувати ваші дані в YAML або використати інший пакет, якщо такий є. Або, простіше, керуйте перетвореннями ваших даних і виведіть їх як рядки, які відповідають синтаксису YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // Вивід: name: Jane Doe
                             //         age: 29
}
```

Це примітивний підхід і може не підійти для складних структур даних або особливих функцій YAML. Для складніших потреб, можливо, доведеться шукати або допомагати в розробці більш повної бібліотеки Dart.
