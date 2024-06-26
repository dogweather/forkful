---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:45.003682-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Dart \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\
  \u0435\u0442 \u043F\u0430\u043A\u0435\u0442 `http`, \u043F\u043E\u043F\u0443\u043B\
  \u044F\u0440\u043D\u0443\u044E \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044E\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F\
  \ \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F HTTP-\u0437\u0430\
  \u043F\u0440\u043E\u0441\u043E\u0432. \u0412\u043E\u0442 \u0431\u0430\u0437\u043E\
  \u0432\u044B\u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u044F \u044D\u0442\u043E\u0433\u043E\
  \u2026"
lastmod: '2024-03-13T22:44:44.509228-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\
  \u0442 \u043F\u0430\u043A\u0435\u0442 `http`, \u043F\u043E\u043F\u0443\u043B\u044F\
  \u0440\u043D\u0443\u044E \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044E\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F\
  \ \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F HTTP-\u0437\u0430\
  \u043F\u0440\u043E\u0441\u043E\u0432."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Dart предоставляет пакет `http`, популярную стороннюю библиотеку для выполнения HTTP-запросов. Вот базовый пример использования этого пакета для скачивания веб-страницы:

Сначала добавьте пакет `http` в файл `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Затем импортируйте пакет и используйте его для получения содержимого веб-страницы:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Страница загружена:');
    print(response.body);
  } else {
    print('Запрос не выполнен со статусом: ${response.statusCode}.');
  }
}
```

**Пример вывода** (это будет меняться в зависимости от содержимого веб-страницы):

```
Страница загружена:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

Для более сложных сценариев, таких как управление куки или установка пользовательских заголовков, вы бы использовали тот же пакет `http`, но с дополнительными настройками вашего запроса:

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
    print('Страница загружена с пользовательскими заголовками:');
    print(response.body);
  } else {
    print('Запрос не выполнен со статусом: ${response.statusCode}.');
  }
}
```

Использование таких заголовков может более точно имитировать запросы браузера, что особенно полезно при работе с сайтами, имеющими конкретные требования или защиту от скрейпинга.
