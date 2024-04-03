---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:52.919936-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Dart \u043F\
  \u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043F\u0430\u043A\u0435\u0442 `http`,\
  \ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443 \u0441\u0442\u043E\u0440\
  \u043E\u043D\u043D\u044E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ \u0434\u043B\u044F \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F HTTP-\u0437\
  \u0430\u043F\u0438\u0442\u0456\u0432. \u041E\u0441\u044C \u0431\u0430\u0437\u043E\
  \u0432\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u044F\u043A \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438\
  \ \u0439\u043E\u0433\u043E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:48.795207-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043F\u0430\u043A\
  \u0435\u0442 `http`, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443 \u0441\
  \u0442\u043E\u0440\u043E\u043D\u043D\u044E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A\u0443 \u0434\u043B\u044F \u0432\u0438\u043A\u043E\u043D\u0430\u043D\
  \u043D\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

## Як зробити:
Dart пропонує пакет `http`, популярну сторонню бібліотеку для виконання HTTP-запитів. Ось базовий приклад, як використовувати його для завантаження веб-сторінки:

Спочатку, додайте пакет `http` до вашого `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Потім імпортуйте пакет та використовуйте його для отримання вмісту веб-сторінки:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Сторінка завантажена:');
    print(response.body);
  } else {
    print('Запит не вдався зі статусом: ${response.statusCode}.');
  }
}
```

**Приклад виводу** (це буде варіюватись в залежності від вмісту веб-сторінки):

```
Сторінка завантажена:
<!doctype html>
<html>
<head>
    <title>Приклад Домену</title>
...
</html>
```

Для більш складних сценаріїв, як-от обробка cookies або встановлення заголовків user-agent, ви б використовували той самий пакет `http`, але з додатковими налаштуваннями для вашого запиту:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Сторінка завантажена з користувацькими заголовками:');
    print(response.body);
  } else {
    print('Запит не вдався зі статусом: ${response.statusCode}.');
  }
}
```

Використання заголовків, як ці, може точніше імітувати запити браузера, що особливо корисно при роботі з сайтами, які мають специфічні вимоги або захист від скрапінгу.
