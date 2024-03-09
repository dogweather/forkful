---
title:                "Розбір дати з рядка"
date:                  2024-03-08T21:56:02.201416-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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