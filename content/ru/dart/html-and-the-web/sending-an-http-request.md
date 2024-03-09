---
title:                "Отправка HTTP-запроса"
date:                  2024-03-08T21:56:17.856216-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
