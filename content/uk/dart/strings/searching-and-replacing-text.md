---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:27.229795-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Dart \u043D\u0430\u0434\u0430\u0454 \u043D\u0430\u0434\u0456\u0439\u043D\u0456\
  \ \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043F\u043E\u0448\u0443\
  \u043A\u0443 \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\u0438 \u0442\u0435\u043A\
  \u0441\u0442\u0443 \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\
  \u044C\u043E \u0447\u0435\u0440\u0435\u0437 \u0439\u043E\u0433\u043E \u043A\u043B\
  \u0430\u0441 `String`, \u0431\u0435\u0437 \u043D\u0435\u043E\u0431\u0445\u0456\u0434\
  \u043D\u043E\u0441\u0442\u0456 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\
  \u043D\u043D\u044F \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u0456\u0445\u2026"
lastmod: '2024-03-13T22:44:48.769974-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043D\u0430\u0434\u0430\u0454 \u043D\u0430\u0434\u0456\u0439\u043D\
  \u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043F\u043E\u0448\
  \u0443\u043A\u0443 \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\u0438 \u0442\u0435\
  \u043A\u0441\u0442\u0443 \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\
  \u043D\u044C\u043E \u0447\u0435\u0440\u0435\u0437 \u0439\u043E\u0433\u043E \u043A\
  \u043B\u0430\u0441 `String`, \u0431\u0435\u0437 \u043D\u0435\u043E\u0431\u0445\u0456\
  \u0434\u043D\u043E\u0441\u0442\u0456 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u0430\u043D\u043D\u044F \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u0456\u0445\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## Як це зробити:
Dart надає надійні методи для пошуку та заміни тексту безпосередньо через його клас `String`, без необхідності використання зовнішніх бібліотек. Ось як ви можете це зробити:

### Базовий пошук та заміна
Для пошуку підрядка та його заміни іншим рядком, можна використовувати `replaceAll`:

```dart
String sampleText = "Привіт, Dart! Dart чудовий.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Результат: Привіт, Flutter! Flutter чудовий.
```

### Використання регулярних виразів
Для складніших потреб у пошуку та заміні Dart використовує регулярні вирази за допомогою класу `RegExp`. Це дозволяє здійснювати пошук і заміну за патерном у рядках:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Результат: Dart 2024, Flutter 2024
```

Цей приклад знаходить усі випадки однієї чи більше цифр (`\d+`) у рядку і замінює їх на "2024".

### Пошук без врахування регістру
Для пошуку без врахування регістру можна змінити конструктор `RegExp`, ігноруючи регістр:

```dart
String sampleText = "Ласкаво просимо до Dart, мови програмування.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Результат: Ласкаво просимо до Flutter, мови програмування.
```

### Заміна за допомогою функції
Для динамічної заміни на основі самого збігу Dart дозволяє передавати функцію в `replaceAllMapped`. Ця функція може виконувати операції або розрахунки на знайдених послідовностях:

```dart
String sampleText = "Збільш 5 на 1 і отримай 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Результат: Збільш 6 на 1 і отримай 7.
```

Ця заміна замінює кожну послідовність цифр на її інкрементоване значення. Кожен збіг перетворюється в ціле число, інкрементується, а потім повертається назад у рядок для заміни.

Можливості Dart з маніпуляції рядками, особливо для пошуку та заміни тексту, роблять його потужним інструментом для обробки та підготовки даних у вашому додатку. Незалежно від того, чи використовуєте ви прямі заміни рядків або скористайтеся потужністю регулярних виразів, Dart надає гнучкість і продуктивність, необхідні для ефективної маніпуляції текстом.
