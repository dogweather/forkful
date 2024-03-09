---
title:                "Завантаження веб-сторінки"
date:                  2024-03-08T21:54:52.919936-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Завантаження веб-сторінки передбачає отримання вмісту веб-сторінки через її URL для обробки або зберігання. Програмісти роблять це, щоб витягти інформацію, стежити за змінами або архівувати контент, що робить це ключовим елементом у веб-скрапінгу, майнінгу даних та автоматизованому тестуванні.

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
