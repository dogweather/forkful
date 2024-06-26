---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:32.809525-07:00
description: "\u042F\u043A: Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\
  \u0430\u043A\u0435\u0442 `http`, \u044F\u043A\u0438\u0439 \u0454 \u043F\u043E\u0442\
  \u0443\u0436\u043D\u0438\u043C \u0456 \u0437\u0440\u0443\u0447\u043D\u0438\u043C\
  \ \u0441\u043F\u043E\u0441\u043E\u0431\u043E\u043C \u0440\u043E\u0431\u043E\u0442\
  \u0438 \u0437 HTTP-\u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C\u0438. \u0421\
  \u043F\u043E\u0447\u0430\u0442\u043A\u0443 \u0432\u043A\u043B\u044E\u0447\u0456\u0442\
  \u044C \u0439\u043E\u0433\u043E \u0443 \u0444\u0430\u0439\u043B pubspec.yaml."
lastmod: '2024-03-13T22:44:48.791775-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\u0430\u043A\u0435\
  \u0442 `http`, \u044F\u043A\u0438\u0439 \u0454 \u043F\u043E\u0442\u0443\u0436\u043D\
  \u0438\u043C \u0456 \u0437\u0440\u0443\u0447\u043D\u0438\u043C \u0441\u043F\u043E\
  \u0441\u043E\u0431\u043E\u043C \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 HTTP-\u0440\
  \u0435\u0441\u0443\u0440\u0441\u0430\u043C\u0438."
title: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F HTTP-\u0437\
  \u0430\u043F\u0438\u0442\u0443"
weight: 44
---

## Як:
Dart включає пакет `http`, який є потужним і зручним способом роботи з HTTP-ресурсами. Спочатку включіть його у файл pubspec.yaml:

```yaml
dependencies:
  http: ^0.13.3
```

Потім імпортуйте його у ваш код Dart, щоб почати виконувати запити:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Тіло відповіді: ${response.body}');
  } else {
    print('Запит завершився з помилкою: ${response.statusCode}.');
  }
}
```

Приклад виводу для успішного запиту може виглядати так:

```
Тіло відповіді: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Для більш складних запитів, наприклад, POST-запитів з JSON-тілом, ви б робили наступне:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('Статус відповіді: ${response.statusCode}');
    print('Тіло відповіді: ${response.body}');
  } else {
    print('Не вдалося створити новий пост. Статус: ${response.statusCode}');
  }
}
```

Приклад виводу для POST-запиту може бути:

```
Статус відповіді: 201
Тіло відповіді: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Ці приклади демонструють базові HTTP GET та POST запити за допомогою пакета `http` в Dart. Цей пакет покриває більшість потреб у надсиланні HTTP-запитів, включаючи більш складні сценарії з заголовками та тілом контенту.
