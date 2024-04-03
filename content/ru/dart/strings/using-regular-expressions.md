---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:09.313388-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Dart \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ \u043A\u043B\u0430\u0441\u0441 `RegExp` \u0434\u043B\u044F \u0440\u0435\u0433\u0443\
  \u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\
  \u0439. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u0440\
  \u0438\u043C\u0435\u0440 \u0434\u043B\u044F \u043F\u043E\u0438\u0441\u043A\u0430\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E \u0448\u0430\u0431\u043B\u043E\
  \u043D\u0430 \u0432 \u0441\u0442\u0440\u043E\u043A\u0435."
lastmod: '2024-03-13T22:44:44.491551-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u043A\
  \u043B\u0430\u0441\u0441 `RegExp` \u0434\u043B\u044F \u0440\u0435\u0433\u0443\u043B\
  \u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\u0439\
  ."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как это сделать:
Dart использует класс `RegExp` для регулярных выражений. Вот простой пример для поиска простого шаблона в строке:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Изучение программирования на Dart увлекательно.';

  if (pattern.hasMatch(text)) {
    print('Совпадение найдено!');
  } else {
    print('Совпадений не найдено.');
  }
  // Вывод: Совпадение найдено!
}
```

Для извлечения совпадений из строки вы можете использовать метод `allMatches`. Этот метод возвращает итерируемую коллекцию совпадений:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart - это здорово!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Это печатает подстроки совпадений.
  }
  // Вывод:
  // Dart
  // это
  // здорово
}
```

Заменить текст можно с помощью методов `replaceFirst` или `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart - это не просто дротик.';
  
  // Заменить первое вхождение
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Вывод: Flutter - это не просто дротик.

  // Заменить все вхождения
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Вывод: Flutter - это не просто flutter.
}
```

Разделение строки по шаблону регулярного выражения просто с использованием метода `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Соответствует любому пробельному символу
  var text = 'Dart весело';

  var parts = text.split(pattern);
  print(parts); 
  // Вывод: [Dart, весело]
}
```

Для сложного анализа или валидации, не поддерживаемых напрямую `RegExp` Dart, вы можете рассмотреть возможность использования сторонних библиотек, но стандартная библиотека Dart часто бывает достаточной для общих задач с регулярными выражениями, подчеркивая ее полезность и универсальность в работе с регулярными выражениями.
