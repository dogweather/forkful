---
title:                "Видалення лапок з рядка"
date:                  2024-03-08T21:56:04.142385-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Видалення лапок з рядка в Dart передбачає вилучення подвійних (") або одинарних (') лапок на початку та в кінці рядка, що корисно для очищення даних або підготовки рядків для подальшої обробки. Програмісти роблять це, щоб нормалізувати вхідні дані, забезпечити єдність у зберіганні даних або при взаємодії з API, які можуть повертати дані у форматі з лапками.

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
