---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:27.308708-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Dart \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043F\u0430\u043A\
  \u0435\u0442 `http` \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\
  \u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0441 \u0431\u0430\
  \u0437\u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\
  \u043A\u0430\u0446\u0438\u0435\u0439. \u0421\u043D\u0430\u0447\u0430\u043B\u0430\
  \ \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435 \u043F\u0430\u043A\u0435\u0442\
  \ `http` \u0432 \u0444\u0430\u0439\u043B\u2026"
lastmod: '2024-03-13T22:44:44.511097-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Dart \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043F\u0430\u043A\u0435\
  \u0442 `http` \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0438\
  \ HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0441 \u0431\u0430\u0437\
  \u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\
  \u0430\u0446\u0438\u0435\u0439."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
В Dart вы можете использовать пакет `http` для отправки HTTP-запросов с базовой аутентификацией. Сначала добавьте пакет `http` в файл `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.4
```

Затем импортируйте пакет в ваш Dart файл:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Чтобы отправить GET-запрос с базовой аутентификацией, вы можете использовать следующий код:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Данные пользователя получены успешно!');
    print('Тело ответа: ${response.body}');
  } else {
    print('Не удалось получить данные пользователя с кодом статуса: ${response.statusCode}');
  }
}
```

Этот код отправляет GET-запрос на 'https://yourapi.com/userdata' с заголовком базовой аутентификации. Имя пользователя и пароль кодируются в base64 и передаются в заголовке 'Authorization' в соответствии со стандартами базовой аутентификации.

**Пример вывода:**

При успешном запросе и если сервер возвращает код состояния 200, вы можете увидеть:

```plaintext
Данные пользователя получены успешно!
Тело ответа: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Если аутентификация не удастся или возникнет какая-либо другая ошибка, код состояния ответа поможет определить проблему.
