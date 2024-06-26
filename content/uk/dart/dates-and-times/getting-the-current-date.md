---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:21.611070-07:00
description: "\u042F\u043A: \u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430 Dart \u043D\u0430\u0434\u0430\u0454\
  \ \u043F\u0440\u044F\u043C\u0438\u0439 \u0434\u043E\u0441\u0442\u0443\u043F \u0434\
  \u043E \u043F\u043E\u0442\u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438\
  \ \u0442\u0430 \u0447\u0430\u0441\u0443 \u0447\u0435\u0440\u0435\u0437 \u043A\u043B\
  \u0430\u0441 `DateTime`. \u041E\u0441\u044C \u0431\u0430\u0437\u043E\u0432\u0438\
  \u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u043E\u0442\u0440\u0438\u043C\
  \u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\u0447\u043D\u043E\u0457 \u0434\
  \u0430\u0442\u0438."
lastmod: '2024-03-13T22:44:48.816707-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u0430 Dart \u043D\u0430\u0434\u0430\u0454 \u043F\u0440\u044F\
  \u043C\u0438\u0439 \u0434\u043E\u0441\u0442\u0443\u043F \u0434\u043E \u043F\u043E\
  \u0442\u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\
  \u0430\u0441\u0443 \u0447\u0435\u0440\u0435\u0437 \u043A\u043B\u0430\u0441 `DateTime`."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## Як:
Основна бібліотека Dart надає прямий доступ до поточної дати та часу через клас `DateTime`. Ось базовий приклад отримання поточної дати:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Приклад виводу: 2023-04-12 10:00:00.000
}
```

Якщо вам потрібна лише частина дати (рік, місяць, день), ви можете відформатувати об’єкт `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Приклад виводу: 2023-04-12
}
```

Dart не включає вбудовану бібліотеку для більш складного форматування дат, але ви можете використовувати пакет `intl` для цієї мети. Спочатку додайте пакет до вашого `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Потім ви можете легко форматувати дати:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Приклад виводу: 2023-04-12
}
```

Для більш розширених варіантів форматування дослідіть клас `DateFormat`, який надається пакетом `intl`, що підтримує широкий спектр шаблонів та локалей.
