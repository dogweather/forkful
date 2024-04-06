---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:59.639682-07:00
description: "\u041A\u0430\u043A: Dart \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\
  \u0432\u043B\u044F\u0435\u0442 \u043F\u0440\u043E\u0441\u0442\u044B\u0435 \u0441\
  \u043F\u043E\u0441\u043E\u0431\u044B \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u044F\
  \ \u043A\u0430\u0432\u044B\u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\
  \u043A\u0438 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0445\
  \ \u0441\u0442\u0440\u043E\u043A\u043E\u0432\u044B\u0445 \u043C\u0435\u0442\u043E\
  \u0434\u043E\u0432 \u0431\u0435\u0437 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\
  \u0438\u043C\u043E\u0441\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0438\
  \u0445 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A.\u2026"
lastmod: '2024-03-13T22:44:44.487606-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\
  \u0442 \u043F\u0440\u043E\u0441\u0442\u044B\u0435 \u0441\u043F\u043E\u0441\u043E\
  \u0431\u044B \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u044F \u043A\u0430\u0432\
  \u044B\u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u0441\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \u043C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0445 \u0441\u0442\
  \u0440\u043E\u043A\u043E\u0432\u044B\u0445 \u043C\u0435\u0442\u043E\u0434\u043E\u0432\
  \ \u0431\u0435\u0437 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u043E\
  \u0441\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0438\u0445 \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 9
---

## Как:
Dart предоставляет простые способы удаления кавычек из строки с использованием встроенных строковых методов без необходимости сторонних библиотек.

### Пример 1: Использование `replaceFirst` и `replaceAll`
Если вы работаете со строками, которые начинаются и заканчиваются кавычками, вы можете использовать методы `replaceFirst` и `replaceAll` для их удаления.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// Удаление двойных кавычек
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Вывод: Hello, World!

// Удаление одинарных кавычек
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Вывод: Dart Programming
```

### Пример 2: Использование `substring`
Этот метод полезен, когда вы уверены, что кавычки находятся только в начале и в конце строки.

```dart
String quotedString = '"Flutter Development"';
// Проверка, начинается и заканчивается ли строка кавычками перед удалением, чтобы избежать ошибок
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Вывод: Flutter Development
```

### Пример 3: Пользовательский метод расширения
Для большей переиспользуемости, особенно если ваш проект часто включает удаление кавычек, рассмотрите возможность создания пользовательского расширения для `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // Вывод: This is Dart
  print(singleQuoted.unquote()); // Вывод: This is awesome
}
```

Эти подходы должны помочь вам эффективно удалять кавычки из строк в Dart, повышая эффективность обработки и подготовки данных.
