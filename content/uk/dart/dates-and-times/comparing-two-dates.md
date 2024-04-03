---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:37.374022-07:00
description: "\u042F\u043A: \u0423 Dart \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\
  \u0435 \u043F\u043E\u0440\u0456\u0432\u043D\u044E\u0432\u0430\u0442\u0438 \u0434\
  \u0430\u0442\u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E \u043A\u043B\u0430\u0441\u0443 `DateTime`, \u044F\u043A\u0438\u0439 \u043F\
  \u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043C\u0435\u0442\u043E\u0434\u0438\
  , \u0442\u0430\u043A\u0456 \u044F\u043A `isBefore`, `isAfter` \u0442\u0430 `isAtSameMomentAs`\
  \ \u0434\u043B\u044F \u043F\u0440\u044F\u043C\u043E\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:48.820341-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Dart \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043F\u043E\
  \u0440\u0456\u0432\u043D\u044E\u0432\u0430\u0442\u0438 \u0434\u0430\u0442\u0438\
  \ \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043A\u043B\
  \u0430\u0441\u0443 `DateTime`, \u044F\u043A\u0438\u0439 \u043F\u0440\u043E\u043F\
  \u043E\u043D\u0443\u0454 \u043C\u0435\u0442\u043E\u0434\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A `isBefore`, `isAfter` \u0442\u0430 `isAtSameMomentAs` \u0434\
  \u043B\u044F \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u043F\u043E\u0440\u0456\
  \u0432\u043D\u044F\u043D\u043D\u044F."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як:
У Dart ви можете порівнювати дати за допомогою класу `DateTime`, який пропонує методи, такі як `isBefore`, `isAfter` та `isAtSameMomentAs` для прямого порівняння. Крім того, різницю між датами можна визначити за допомогою методу `difference()`, який надає об'єкт `Duration`, деталізуючи проміжок часу між двома точками у часі.

Ось простий приклад, що ілюструє ці концепції:

```dart
void main() {
  DateTime eventStart = DateTime(2023, 5, 15);
  DateTime eventEnd = DateTime(2023, 5, 20);
  
  // Перевірка, чи одна дата передує іншій
  if (eventStart.isBefore(eventEnd)) {
    print("Дата початку події є до дати завершення події.");
  }

  // Перевірка, чи дві дати є однаковими
  if (!eventStart.isAtSameMomentAs(eventEnd)) {
    print("Дата початку та завершення події не співпадають.");
  }
  
  // Розрахунок різниці між двома датами
  Duration eventDuration = eventEnd.difference(eventStart);
  print("Подія триває ${eventDuration.inDays} днів.");
}

/*
Вивід:
Дата початку події є до дати завершення події.
Дата початку та завершення події не співпадають.
Подія триває 5 днів.
*/
```

Для більш розширеної маніпуляції з датами, такої як конвертація форматів, може бути корисним клас `DateFormat` з пакету `intl`. Нижче представлений приклад, який демонструє як використовувати його для форматування та порівняння дат:

Спочатку включіть пакет `intl` у ваш `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Потім використовуйте його таким чином:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime departureDate = DateTime(2023, 5, 15);
  DateTime returnDate = DateTime.parse('2023-05-20');

  // Форматування дат
  var formatter = DateFormat('yyyy-MM-dd');
  print("Відправлення: ${formatter.format(departureDate)}");
  print("Повернення: ${formatter.format(returnDate)}");

  // Порівняння за допомогою відформатованих рядків
  if (formatter.format(departureDate) == formatter.format(returnDate)) {
    print("Дата відправлення та повернення співпадають.");
  } else {
    print("Дата відправлення та повернення відрізняються.");
  }
}

/*
Вивід:
Відправлення: 2023-05-15
Повернення: 2023-05-20
Дата відправлення та повернення відрізняються.
*/
```

Цей приклад демонструє, як порівнювати два об'єкти `DateTime` безпосередньо та за допомогою відформатованих рядків для порівнянь, які потребують ігнорування певних компонентів, наприклад, часу.
