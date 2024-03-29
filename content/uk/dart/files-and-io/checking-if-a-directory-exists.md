---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:07.667104-07:00
description: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u043A\u0430\u0442\u0430\u043B\u043E\u0433\u0443\
  \ \u0432 Dart \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0432 \u0442\u043E\u043C\
  \u0443, \u0449\u043E\u0431 \u043F\u0456\u0434\u0442\u0432\u0435\u0440\u0434\u0438\
  \u0442\u0438 \u043F\u0440\u0438\u0441\u0443\u0442\u043D\u0456\u0441\u0442\u044C\
  \ \u043A\u0430\u0442\u0430\u043B\u043E\u0433\u0443 \u0437\u0430 \u0432\u043A\u0430\
  \u0437\u0430\u043D\u0438\u043C \u0448\u043B\u044F\u0445\u043E\u043C \u0443 \u0444\
  \u0430\u0439\u043B\u043E\u0432\u0456\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0456\
  \ \u043F\u0435\u0440\u0435\u0434 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\
  \u044F\u043C \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0439,\u2026"
lastmod: '2024-03-13T22:44:48.824097-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u043A\u0430\u0442\u0430\u043B\u043E\u0433\u0443\
  \ \u0432 Dart \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0432 \u0442\u043E\u043C\
  \u0443, \u0449\u043E\u0431 \u043F\u0456\u0434\u0442\u0432\u0435\u0440\u0434\u0438\
  \u0442\u0438 \u043F\u0440\u0438\u0441\u0443\u0442\u043D\u0456\u0441\u0442\u044C\
  \ \u043A\u0430\u0442\u0430\u043B\u043E\u0433\u0443 \u0437\u0430 \u0432\u043A\u0430\
  \u0437\u0430\u043D\u0438\u043C \u0448\u043B\u044F\u0445\u043E\u043C \u0443 \u0444\
  \u0430\u0439\u043B\u043E\u0432\u0456\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0456\
  \ \u043F\u0435\u0440\u0435\u0434 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\
  \u044F\u043C \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0439,\u2026"
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
---

{{< edit_this_page >}}

## Що і чому?

Перевірка наявності каталогу в Dart полягає в тому, щоб підтвердити присутність каталогу за вказаним шляхом у файловій системі перед виконанням операцій, таких як читання або запис файлів. Програмісти роблять це, щоб уникнути помилок, які виникають при спробі доступу або модифікації каталогів, яких не існує.

## Як це зробити:

Dart використовує бібліотеку `dart:io` для роботи з файлами та каталогами. Ось простий спосіб перевірити наявність каталогу:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('Каталог існує');
  } else {
    print('Каталог не існує');
  }
}
```
Приклад виведення, якщо каталог існує:
```
Каталог існує
```

Або, якщо ні:
```
Каталог не існує
```

Для обробки більш складних сценаріїв, таких як асинхронна перевірка або створення каталогу, якщо він не існує, ви можете використати такий підхід:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // Асинхронно перевіряємо, чи каталог існує
  var exists = await directory.exists();
  if (exists) {
    print('Каталог існує');
  } else {
    print('Каталог не існує, створюємо...');
    await directory.create(); // Це створює каталог
    print('Каталог створено');
  }
}
```

Приклад виведення, якщо каталог не існував і був створений:
```
Каталог не існує, створюємо...
Каталог створено
```

Вбудовані можливості Dart зазвичай достатні для роботи з файлами та каталогами, тому сторонні бібліотеки зазвичай не потрібні для цього завдання. Однак, для більш складних операцій з файловою системою, пакети, такі як `path` (для маніпуляцій зі шляхами у спосіб, незалежний від платформи), можуть доповнювати бібліотеку `dart:io`, але не пропонують прямих більш продвинутих перевірок наявності каталогів, ніж те, що показано.
