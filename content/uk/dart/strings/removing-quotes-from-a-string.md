---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:04.142385-07:00
description: "\u042F\u043A: Dart \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\u044F\
  \u043C\u043E\u043B\u0456\u043D\u0456\u0439\u043D\u0456 \u0441\u043F\u043E\u0441\u043E\
  \u0431\u0438 \u0432\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\
  \u043F\u043E\u043A \u0437 \u0440\u044F\u0434\u043A\u0430, \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0432\u0431\u0443\u0434\
  \u043E\u0432\u0430\u043D\u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0440\u044F\
  \u0434\u043A\u0430 \u0431\u0435\u0437 \u043F\u043E\u0442\u0440\u0435\u0431\u0438\
  \ \u0432 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0430\u0445."
lastmod: '2024-04-05T21:53:49.010661-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\u044F\u043C\u043E\u043B\
  \u0456\u043D\u0456\u0439\u043D\u0456 \u0441\u043F\u043E\u0441\u043E\u0431\u0438\
  \ \u0432\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\u043F\u043E\
  \u043A \u0437 \u0440\u044F\u0434\u043A\u0430, \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0440\u044F\u0434\u043A\
  \u0430 \u0431\u0435\u0437 \u043F\u043E\u0442\u0440\u0435\u0431\u0438 \u0432 \u0441\
  \u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A\u0430\u0445."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u043B\u0430\u043F\u043E\
  \u043A \u0437 \u0440\u044F\u0434\u043A\u0430"
weight: 9
---

## Як:
Dart надає прямолінійні способи видалення лапок з рядка, використовуючи вбудовані методи рядка без потреби в сторонніх бібліотеках.

### Приклад 1: Використання `replaceFirst` і `replaceAll`
Якщо ви працюєте з рядками, що починаються і закінчуються лапками, ви можете використовувати методи `replaceFirst` і `replaceAll` для їх видалення.

```dart
String quotedString = '"Привіт, світ!"';
String singleQuotedString = '\'Програмування Dart\'';

// Видалення подвійних лапок
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Вивід: Привіт, світ!

// Видалення одинарних лапок
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Вивід: Програмування Dart
```

### Приклад 2: Використання `substring`
Цей метод корисний, коли ви впевнені, що лапки знаходяться саме на початку і в кінці рядка.

```dart
String quotedString = '"Розробка на Flutter"';
// Перевірте, чи починається і закінчується цими лапками, перш ніж видаляти, щоб уникнути помилок
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Вивід: Розробка на Flutter
```

### Приклад 3: Власний метод розширення
Для більшої повторюваності використання, особливо якщо ваш проєкт часто включає видалення лапок, розгляньте створення власного розширення для `String`.

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
  String doubleQuoted = '"Це Dart"';
  String singleQuoted = '\'Це чудово\'';
  print(doubleQuoted.unquote()); // Вивід: Це Dart
  print(singleQuoted.unquote()); // Вивід: Це чудово
}
```

Ці підходи допоможуть вам ефективно видаляти лапки з рядків у Dart, покращуючи ваші процеси обробки і підготовки даних.
