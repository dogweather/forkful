---
title:                "Перевірка наявності директорії"
date:                  2024-03-08T21:54:07.667104-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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