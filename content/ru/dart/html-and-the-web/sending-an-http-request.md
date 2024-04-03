---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.856216-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u043F\u0430\
  \u043A\u0435\u0442 `http`, \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0438 \u043C\
  \u043E\u0449\u043D\u044B\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0440\u0430\
  \u0431\u043E\u0442\u044B \u0441 HTTP-\u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C\
  \u0438. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0432\u043A\u043B\u044E\u0447\
  \u0438\u0442\u0435 \u0435\u0433\u043E \u0432 \u0432\u0430\u0448 \u0444\u0430\u0439\
  \u043B pubspec.yaml."
lastmod: '2024-03-13T22:44:44.505662-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u043F\u0430\u043A\
  \u0435\u0442 `http`, \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0438 \u043C\u043E\
  \u0449\u043D\u044B\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0440\u0430\u0431\
  \u043E\u0442\u044B \u0441 HTTP-\u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C\u0438\
  ."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

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
