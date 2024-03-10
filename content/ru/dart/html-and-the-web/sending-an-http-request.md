---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.856216-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u043D\u0430 Dart - \u044D\u0442\u043E \u043F\u0440\u043E\
  \u0446\u0435\u0441\u0441 \u0438\u043D\u0438\u0446\u0438\u0430\u0446\u0438\u0438\
  \ \u0441\u0432\u044F\u0437\u0438 \u0441 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\
  \u0435\u0440\u043E\u043C \u0438\u043B\u0438 API \u0438\u0437 \u043F\u0440\u0438\u043B\
  \u043E\u0436\u0435\u043D\u0438\u044F Dart. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\
  \u043E \u0434\u043B\u044F \u0442\u043E\u0433\u043E, \u0447\u0442\u043E\u0431\u044B\
  \ \u0438\u0437\u0432\u043B\u0435\u043A\u0430\u0442\u044C\u2026"
lastmod: '2024-03-09T21:06:08.902206-07:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u043D\u0430 Dart - \u044D\u0442\u043E \u043F\u0440\u043E\
  \u0446\u0435\u0441\u0441 \u0438\u043D\u0438\u0446\u0438\u0430\u0446\u0438\u0438\
  \ \u0441\u0432\u044F\u0437\u0438 \u0441 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\
  \u0435\u0440\u043E\u043C \u0438\u043B\u0438 API \u0438\u0437 \u043F\u0440\u0438\u043B\
  \u043E\u0436\u0435\u043D\u0438\u044F Dart. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\
  \u043E \u0434\u043B\u044F \u0442\u043E\u0433\u043E, \u0447\u0442\u043E\u0431\u044B\
  \ \u0438\u0437\u0432\u043B\u0435\u043A\u0430\u0442\u044C\u2026"
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса на Dart - это процесс инициации связи с веб-сервером или API из приложения Dart. Программисты делают это для того, чтобы извлекать данные с веба, отправлять формы и взаимодействовать с RESTful-сервисами, что делает его фундаментальной операцией для разработки веб, серверных и мобильных приложений на Dart.

## Как это сделать:

Dart включает пакет `http`, удобный и мощный способ работы с HTTP-ресурсами. Сначала включите его в ваш файл pubspec.yaml:

```yaml
dependencies:
  http: ^0.13.3
```

Затем импортируйте его в ваш Dart-код, чтобы начать отправку запросов:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Тело ответа: ${response.body}');
  } else {
    print('Запрос не удался со статусом: ${response.statusCode}.');
  }
}
```

Пример вывода для успешного запроса может выглядеть так:

```
Тело ответа: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Для более сложных запросов, таких как POST-запросы с телом в формате JSON, вы бы сделали следующее:

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
    print('Статус ответа: ${response.statusCode}');
    print('Тело ответа: ${response.body}');
  } else {
    print('Не удалось создать новый пост. Статус: ${response.statusCode}');
  }
}
```

Пример вывода для POST-запроса может быть:

```
Статус ответа: 201
Тело ответа: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Эти примеры демонстрируют базовые HTTP GET и POST запросы с использованием пакета `http` в Dart. Этот пакет покрывает большинство потребностей по отправке HTTP-запросов, включая более сложные сценарии с заголовками и телом содержимого.
