---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:06.821806-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: Dart\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454 \u043A\
  \u043B\u0430\u0441 `RegExp` \u0434\u043B\u044F \u0440\u0435\u0433\u0443\u043B\u044F\
  \u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\u0456\u0432. \u041E\u0441\
  \u044C \u0431\u0430\u0437\u043E\u0432\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\
  \u0430\u0434 \u043F\u043E\u0448\u0443\u043A\u0443 \u0437\u0430 \u043F\u0440\u043E\
  \u0441\u0442\u0438\u043C \u0448\u0430\u0431\u043B\u043E\u043D\u043E\u043C \u0443\
  \ \u0440\u044F\u0434\u043A\u0443."
lastmod: '2024-03-13T22:44:48.779225-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\
  \ \u043A\u043B\u0430\u0441 `RegExp` \u0434\u043B\u044F \u0440\u0435\u0433\u0443\u043B\
  \u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\u0456\u0432."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

## Як це робити:
Dart використовує клас `RegExp` для регулярних виразів. Ось базовий приклад пошуку за простим шаблоном у рядку:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Вивчення програмування на Dart захоплює.';

  if (pattern.hasMatch(text)) {
    print('Збіг знайдено!');
  } else {
    print('Збігів не знайдено.');
  }
  // Вивід: Збіг знайдено!
}
```

Щоб витягти збіги з рядка, можна скористатися методом `allMatches`. Цей метод повертає ітерабельну колекцію збігів:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart круто!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Це виводить відповідні підрядки.
  }
  // Вивід:
  // Dart
  // круто
}
```

Замінити текст можна, використовуючи методи `replaceFirst` або `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart не просто дротик.';

  // Заміна першого збігу
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Вивід: Flutter не просто дротик.

  // Заміна всіх збігів
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Вивід: Flutter не просто flutter.
}
```

Розділення рядка за допомогою регекс-шаблону просте з методом `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Відповідає будь-якому символу пробілу
  var text = 'Dart весело';

  var parts = text.split(pattern);
  print(parts); 
  // Вивід: [Dart, весело]
}
```

Для складного парсингу чи валідацій, які не підтримуються безпосередньо регексами в Dart, можна розглянути використання сторонніх бібліотек, але стандартна бібліотека Dart зазвичай є достатньою для поширених регекс-задач, підкреслюючи її корисність і універсальність у роботі з регулярними виразами.
