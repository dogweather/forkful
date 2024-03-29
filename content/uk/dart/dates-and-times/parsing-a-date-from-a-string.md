---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.201416-07:00
description: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437\
  \ \u0440\u044F\u0434\u043A\u0430 \u0432 Dart \u043F\u043E\u043B\u044F\u0433\u0430\
  \u0454 \u0443 \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u0457\
  \ \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u043E\u0433\u043E \u043F\u0440\u0435\
  \u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \ \u0442\u0430 \u0447\u0430\u0441\u0456\u0432 \u0443 \u043E\u0431'\u0454\u043A\u0442\
  \ `DateTime`. \u0426\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u044F \u0454\
  \ \u0432\u0430\u0436\u043B\u0438\u0432\u043E\u044E \u0434\u043B\u044F \u0434\u043E\
  \u0434\u0430\u0442\u043A\u0456\u0432, \u0449\u043E \u043C\u0430\u044E\u0442\u044C\
  \u2026"
lastmod: '2024-03-13T22:44:48.814935-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430 \u0432 Dart \u043F\u043E\u043B\u044F\u0433\u0430\u0454\
  \ \u0443 \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u0457 \u0442\
  \u0435\u043A\u0441\u0442\u043E\u0432\u043E\u0433\u043E \u043F\u0440\u0435\u0434\u0441\
  \u0442\u0430\u0432\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442 \u0442\u0430\
  \ \u0447\u0430\u0441\u0456\u0432 \u0443 \u043E\u0431'\u0454\u043A\u0442 `DateTime`.\
  \ \u0426\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u044F \u0454 \u0432\u0430\
  \u0436\u043B\u0438\u0432\u043E\u044E \u0434\u043B\u044F \u0434\u043E\u0434\u0430\
  \u0442\u043A\u0456\u0432, \u0449\u043E \u043C\u0430\u044E\u0442\u044C\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що і чому?
Розбір дати з рядка в Dart полягає у конвертації текстового представлення дат та часів у об'єкт `DateTime`. Ця операція є важливою для додатків, що мають справу з плануванням, аналізом даних або будь-якою функцією, що вимагає маніпуляцій з датами, забезпечуючи правильне розуміння та обробку даних, пов'язаних з датами, програмою.

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
