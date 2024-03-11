---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:59.639682-07:00
description: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\
  \u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 Dart\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F\
  \ \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0434\u0432\u043E\u0439\u043D\
  \u044B\u0445 (\") \u0438\u043B\u0438 \u043E\u0434\u0438\u043D\u0430\u0440\u043D\u044B\
  \u0445 (') \u043A\u0430\u0432\u044B\u0447\u0435\u043A \u0441 \u043D\u0430\u0447\u0430\
  \u043B\u0430 \u0438 \u043A\u043E\u043D\u0446\u0430 \u0441\u0442\u0440\u043E\u043A\
  \u0438, \u0447\u0442\u043E \u043F\u043E\u043B\u0435\u0437\u043D\u043E \u0434\u043B\
  \u044F \u043E\u0447\u0438\u0441\u0442\u043A\u0438 \u0434\u0430\u043D\u043D\u044B\
  \u0445\u2026"
lastmod: '2024-03-11T00:14:18.085899-06:00'
model: gpt-4-0125-preview
summary: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\
  \u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 Dart\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F\
  \ \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0434\u0432\u043E\u0439\u043D\
  \u044B\u0445 (\") \u0438\u043B\u0438 \u043E\u0434\u0438\u043D\u0430\u0440\u043D\u044B\
  \u0445 (') \u043A\u0430\u0432\u044B\u0447\u0435\u043A \u0441 \u043D\u0430\u0447\u0430\
  \u043B\u0430 \u0438 \u043A\u043E\u043D\u0446\u0430 \u0441\u0442\u0440\u043E\u043A\
  \u0438, \u0447\u0442\u043E \u043F\u043E\u043B\u0435\u0437\u043D\u043E \u0434\u043B\
  \u044F \u043E\u0447\u0438\u0441\u0442\u043A\u0438 \u0434\u0430\u043D\u043D\u044B\
  \u0445\u2026"
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и почему?
Удаление кавычек из строки в Dart включает в себя удаление двойных (") или одинарных (') кавычек с начала и конца строки, что полезно для очистки данных или подготовки строк для дальнейшей обработки. Программисты делают это для нормализации входных данных, обеспечения единообразия в хранении данных или при взаимодействии с API, которые могут возвращать данные в кавычках.

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
