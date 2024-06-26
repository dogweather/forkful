---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:36.123376-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u0423 Dart \u043C\
  \u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0432\u0430\u0442\u0438 \u0440\u0456\u0437\u043D\u0456 \u043C\u0435\u0442\
  \u043E\u0434\u0438 \u0434\u043B\u044F \u0432\u0438\u0434\u043E\u0431\u0443\u0432\
  \u0430\u043D\u043D\u044F \u043F\u0456\u0434\u0440\u044F\u0434\u043A\u0456\u0432\
  , \u0442\u0430\u043A\u0456 \u044F\u043A `substring()`, `split()` \u0442\u0430 \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\u0430\u0437\u0438\
  . \u041A\u043E\u0436\u0435\u043D \u043C\u0435\u0442\u043E\u0434 \u0441\u043B\u0443\
  \u0436\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:48.777395-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Dart \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0440\u0456\u0437\u043D\
  \u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u0432\u0438\u0434\
  \u043E\u0431\u0443\u0432\u0430\u043D\u043D\u044F \u043F\u0456\u0434\u0440\u044F\u0434\
  \u043A\u0456\u0432, \u0442\u0430\u043A\u0456 \u044F\u043A `substring()`, `split()`\
  \ \u0442\u0430 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\
  \u0440\u0430\u0437\u0438."
title: "\u0412\u0438\u0434\u043E\u0431\u0443\u0432\u0430\u043D\u043D\u044F \u043F\u0456\
  \u0434\u0440\u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## Як робити:
У Dart можна використовувати різні методи для видобування підрядків, такі як `substring()`, `split()` та регулярні вирази. Кожен метод служить різним цілям і пропонує гнучкість у роботі з рядками.

### Використання `substring()`:
Метод `substring()` є простим у використанні. Ви вказуєте початковий (і необов’язково кінцевий) індекс для зрізу рядка.

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // Вивід: World
}
```

### Використання `split()`:
Розділіть рядок на список підрядків на основі шаблону (як-от пробіл або кома) та потім отримайте доступ до підрядка за індексом.

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // Доступ за індексом
  print(result); // Вивід: is
}
```

### Використання регулярних виразів:
Для складних шаблонів клас `RegExp` у Dart є потужним. Використовуйте його для пошуку шаблонів і видобування підрядків.

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // Вивід: example@mail.com
}
```

### Сторонні бібліотеки:
Хоча стандартна бібліотека Dart є досить здатною, ви можете зустріти сценарії, де стороння бібліотека може спростити ваше завдання. Популярний вибір для маніпуляції з рядками та порівняння шаблонів тут конкретно не пропагується, оскільки можливості, вбудовані в Dart, часто вистачає. Проте завжди перевіряйте [pub.dev](https://pub.dev) на предмет бібліотек, які можуть краще відповідати вашим конкретним потребам.
