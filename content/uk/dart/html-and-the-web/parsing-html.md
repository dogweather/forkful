---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:49.109533-07:00
description: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u0443 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u0456 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u0432\u0438\u043B\u0443\u0447\u0435\u043D\u043D\u044F \u0434\
  \u0430\u043D\u0438\u0445 \u0437 HTML-\u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\
  \u0456\u0432. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \ \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0432\
  \u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044F\u0442\u0438 \u0437 \u0432\u0435\
  \u0431-\u043A\u043E\u043D\u0442\u0435\u043D\u0442\u043E\u043C \u0430\u0431\u043E\
  \ \u0441\u043A\u0440\u0435\u0439\u043F\u0438\u0442\u0438 \u0439\u043E\u0433\u043E\
  \ \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:48.793493-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u0443 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u0456 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u0432\u0438\u043B\u0443\u0447\u0435\u043D\u043D\u044F \u0434\
  \u0430\u043D\u0438\u0445 \u0437 HTML-\u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\
  \u0456\u0432. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \ \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0432\
  \u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044F\u0442\u0438 \u0437 \u0432\u0435\
  \u0431-\u043A\u043E\u043D\u0442\u0435\u043D\u0442\u043E\u043C \u0430\u0431\u043E\
  \ \u0441\u043A\u0440\u0435\u0439\u043F\u0438\u0442\u0438 \u0439\u043E\u0433\u043E\
  \ \u0434\u043B\u044F\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 HTML"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML у програмуванні означає вилучення даних з HTML-документів. Програмісти роблять це, щоб взаємодіяти з веб-контентом або скрейпити його для витягування інформації, тестування чи автоматизації, навіть коли офіційні API недоступні.

## Як це зробити:
Dart не надає вбудовану підтримку для парсингу HTML у своїх основних бібліотеках. Однак, ви можете використати сторонній пакет, як-от `html`, для парсингу та маніпулювання HTML-документами.

Спочатку додайте пакет `html` до вашого файлу `pubspec.yaml`:

```yaml
dependencies:
  html: ^0.15.0
```

Потім імпортуйте пакет у ваш Dart файл:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Ось базовий приклад парсингу рядка, що містить HTML, і вилучення даних:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Привіт, Dart!</h1>
      <p>Це абзац у зразковому HTML</p>
    </body>
  </html>
  """;

  // Парсинг рядка HTML
  Document document = parse(htmlDocument);

  // Витягування даних
  String title = document.querySelector('h1')?.text ?? "Заголовок не знайдено";
  String paragraph = document.querySelector('p')?.text ?? "Абзац не знайдено";

  print('Заголовок: $title');
  print('Абзац: $paragraph');
}
```

Вивід:

```
Заголовок: Привіт, Dart!
Абзац: Це абзац у зразковому HTML
```

Щоб взаємодіяти з реальними веб-сторінками, ви можете поєднати парсинг `html` з HTTP запитами (використовуючи пакет `http` для отримання веб-контенту). Ось швидкий приклад:

Спочатку додайте пакет `http` разом з `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Потім, отримайте та проаналізуйте HTML-сторінку з мережі:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Отримання вебсторінки
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Припустимо, що сторінка містить теги <h1>, які вас цікавлять
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Заголовки: $headlines');
  } else {
    print('Запит завершився з помилкою: ${response.statusCode}.');
  }
}
```

Примітка: Техніка веб-скрейпінгу, показана вище, має використовуватися відповідально та відповідно до умов обслуговування вебсайту.
