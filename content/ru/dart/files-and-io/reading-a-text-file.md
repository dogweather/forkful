---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:37.031881-07:00
description: "\u041A\u0430\u043A: \u041E\u0441\u043D\u043E\u0432\u043D\u0430\u044F\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 Dart, `dart:io`,\
  \ \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442\
  \ \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u044B\u0435 \u0444\u0443\
  \u043D\u043A\u0446\u0438\u043E\u043D\u0430\u043B\u044C\u043D\u044B\u0435 \u0432\u043E\
  \u0437\u043C\u043E\u0436\u043D\u043E\u0441\u0442\u0438 \u0434\u043B\u044F \u0441\
  \u0438\u043D\u0445\u0440\u043E\u043D\u043D\u043E\u0433\u043E \u0438\u043B\u0438\
  \ \u0430\u0441\u0438\u043D\u0445\u0440\u043E\u043D\u043D\u043E\u0433\u043E \u0447\
  \u0442\u0435\u043D\u0438\u044F \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445\
  \ \u0444\u0430\u0439\u043B\u043E\u0432.\u2026"
lastmod: '2024-03-13T22:44:44.544282-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043D\u043E\u0432\u043D\u0430\u044F \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0430 Dart, `dart:io`, \u043F\u0440\u0435\u0434\u043E\
  \u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u043D\u0435\u043E\u0431\u0445\u043E\
  \u0434\u0438\u043C\u044B\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u043E\u043D\u0430\
  \u043B\u044C\u043D\u044B\u0435 \u0432\u043E\u0437\u043C\u043E\u0436\u043D\u043E\u0441\
  \u0442\u0438 \u0434\u043B\u044F \u0441\u0438\u043D\u0445\u0440\u043E\u043D\u043D\
  \u043E\u0433\u043E \u0438\u043B\u0438 \u0430\u0441\u0438\u043D\u0445\u0440\u043E\
  \u043D\u043D\u043E\u0433\u043E \u0447\u0442\u0435\u043D\u0438\u044F \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u044B\u0445 \u0444\u0430\u0439\u043B\u043E\u0432."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как:
Основная библиотека Dart, `dart:io`, предоставляет необходимые функциональные возможности для синхронного или асинхронного чтения текстовых файлов. Вот как подходить к каждому из них.

**Синхронно:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // Синхронное чтение файла
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Ошибка чтения файла: $e');
  }
}
```

**Асинхронно:**

Чтобы избежать блокирования программы во время чтения файла, особенно полезно для больших файлов или реактивных приложений:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Ошибка чтения файла: $e');
  }
}
```

**Пример вывода:**

Если ваш текстовый файл содержит:

```
Привет, Dart!
```

Оба вышеуказанных метода выведут:

```
Привет, Dart!
```

**Использование сторонней библиотеки:**

Для дополнительных функций, таких как упрощенные операции с файлами или улучшенная обработка ошибок, вы можете рассмотреть сторонние библиотеки, такие как `package:file`. Однако, по состоянию на последнее обновление, использование основного пакета `dart:io`, как показано выше, является наиболее распространенным и простым методом чтения текстовых файлов в Dart.
