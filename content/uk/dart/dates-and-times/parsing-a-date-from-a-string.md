---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.201416-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A\u0430 Dart \u0441\u043F\u0440\u043E\u0449\u0443\u0454 \u0440\u043E\
  \u0437\u0431\u0456\u0440 \u0434\u0430\u0442 \u0437\u0430 \u0434\u043E\u043F\u043E\
  \u043C\u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `DateTime`. \u0414\
  \u043B\u044F \u043F\u0440\u043E\u0441\u0442\u0438\u0445 \u0432\u0438\u043F\u0430\
  \u0434\u043A\u0456\u0432, \u043A\u043E\u043B\u0438 \u0432\u0438 \u0437\u043D\u0430\
  \u0454\u0442\u0435 \u0444\u043E\u0440\u043C\u0430\u0442 \u0440\u044F\u0434\u043A\
  \u0430 \u0437 \u0434\u0430\u0442\u043E\u044E, \u043C\u043E\u0436\u043D\u0430\u2026"
lastmod: '2024-03-13T22:44:48.814935-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u0430 Dart \u0441\u043F\u0440\u043E\u0449\u0443\u0454 \u0440\
  \u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442 \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `DateTime`."
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

## Як це зробити:
Основна бібліотека Dart спрощує розбір дат за допомогою класу `DateTime`. Для простих випадків, коли ви знаєте формат рядка з датою, можна використовувати метод `DateTime.parse()`. Однак для складніших сценаріїв або при роботі з кількома форматами, пакет `intl`, зокрема клас `DateFormat`, стає незамінним.

### Використання основної бібліотеки Dart:
```dart
void main() {
  // Використання DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Використання пакету `intl`:
Спочатку додайте пакет `intl` до вашого файлу `pubspec.yaml`:
```yaml
dependencies:
  intl: ^0.17.0
```
Потім імпортуйте пакет та використовуйте `DateFormat` для розбору:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
Пакет `intl` пропонує потужні опції для розбору дат, дозволяючи безпроблемно обробляти різні міжнародні формати дат.
