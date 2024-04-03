---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:47.832342-07:00
description: "\u042F\u043A: #."
lastmod: '2024-03-13T22:44:48.766284-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u043D\u0430 \u043F\u0440\u043E\u043F\u0438\u0441\u043D\
  \u0456"
weight: 2
---

## Як:


### Використовуючи вбудовані методи Dart
Dart надає прості та зрозумілі методи для маніпуляції рядками. Для того, щоб привести слово або речення до великої літери, вам зазвичай потрібно взяти перший символ, перетворити його на верхній регістр, а потім конкатенувати його з рештою рядка. Ось як це можна реалізувати:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Вивід: Hello world
}
```

### Приведення до великої літери кожного слова
Щоб привести до великої літери першу літеру кожного слова у рядку, ви могли б розділити рядок на слова, привести кожне до великої літери, а потім з'єднати їх назад разом:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Вивід: Hello Dart Enthusiasts
}
```

### Використання сторонніх бібліотек
Хоча стандартна бібліотека Dart покриває базові потреби, деякі завдання можуть бути зручніше виконані за допомогою сторонніх пакетів. Популярним вибором для розширених можливостей маніпуляції рядками, включно з приведенням до великої літери, є пакет [`recase`](https://pub.dev/packages/recase). Додавши його до файлу `pubspec.yaml` вашого проекту, ви можете легко приводити рядки до великої літери серед інших функціональностей:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Вивід: Hello World
}
```

Використовуючи `recase`, ви можете приводити до великої літери окремі слова, цілі речення, або навіть слідувати іншим конвенціям розміщення без необхідності ручної обробки трансформацій рядків.
