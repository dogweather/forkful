---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:15.601245-07:00
description: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0432 Dart -\
  \ \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0437\u0430\u043F\u0438\u0441\
  \u0443 \u0440\u0456\u0437\u043D\u0438\u0445 \u0440\u0456\u0432\u043D\u0456\u0432\
  \ \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u043F\u0456\u0434\
  \ \u0447\u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\
  \u0435 \u0434\u043B\u044F \u043C\u043E\u043D\u0456\u0442\u043E\u0440\u0438\u043D\
  \u0433\u0443 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043D\u043E\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:48.809619-06:00'
model: gpt-4-0125-preview
summary: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0432 Dart - \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0437\u0430\u043F\u0438\u0441\u0443\
  \ \u0440\u0456\u0437\u043D\u0438\u0445 \u0440\u0456\u0432\u043D\u0456\u0432 \u0456\
  \u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u043F\u0456\u0434 \u0447\
  \u0430\u0441 \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  \ \u0434\u043B\u044F \u043C\u043E\u043D\u0456\u0442\u043E\u0440\u0438\u043D\u0433\
  \u0443 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043D\u043E\u0433\u043E\u2026"
title: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F"
---

{{< edit_this_page >}}

## Що і Чому?

Логування в Dart - це процес запису різних рівнів інформації під час виконання програми. Програмісти роблять це для моніторингу поведінки програмного забезпечення, відлагодження проблем та аналізу продуктивності, що полегшує обслуговування та вдосконалення додатку з часом.

## Як це зробити:

Dart включає простий механізм логування через бібліотеку `dart:developer`. Для більш складних потреб логування програмісти часто звертаються до бібліотек третіх сторін, таких як `logger` та `log4dart`.

### Використання `dart:developer`
Це підходить для базового логування, особливо під час розробки:

```dart
import 'dart:developer';

void main() {
  log('Це повідомлення для логування помилок.');
}
```

Вивід:
```
Це повідомлення для логування помилок.
```

### Використання пакету `logger`
Для більш комплексного рішення пакет `logger` пропонує різні рівні логування (наприклад, інформація, попередження, помилка) і може бути відформатований у більш зрозумілий спосіб.

Спочатку додайте залежність `logger` у ваш файл `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

Потім використовуйте його так:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Це повідомлення для дебагу");
  logger.w("Це попереджувальне повідомлення");
  logger.e("Це повідомлення про помилку");
}
```

Приклад виводу може виглядати так, з кожним типом повідомлення, відформатованим по-різному для легкої ідентифікації:

```
💬 Це повідомлення для дебагу
⚠️ Це попереджувальне повідомлення
❗️ Це повідомлення про помилку
```

### Використання пакету `log4dart`
Для додатків, що потребують конфігурації на основі логування (подібно до Log4j), `log4dart` пропонує знайомий підхід. Це особливо корисно для масштабних застосунків.

Переконайтеся, що ви включили `log4dart` у ваш `pubspec.yaml`:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Простий приклад використання:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Відлагодження MyApp");
  logger.info("Інформаційне повідомлення");
}
```

Вивід:

```
DEBUG: Відлагодження MyApp
INFO: Інформаційне повідомлення
```

Кожен із цих методів забезпечує різний рівень гнучкості та складності, від простих повідомлень для дебагу до всебічного, конфігурованого логування, яке відповідає потребам складних застосунків.
