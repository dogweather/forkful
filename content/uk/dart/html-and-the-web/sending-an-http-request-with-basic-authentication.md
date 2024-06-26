---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:03.947451-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u0412 Dart\
  \ \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043F\u0430\u043A\u0435\
  \u0442 `http` \u0434\u043B\u044F \u043D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\
  \u043D\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432 \u0437 \u0431\u0430\
  \u0437\u043E\u0432\u043E\u044E \u0430\u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\
  \u043A\u0430\u0446\u0456\u0454\u044E. \u0421\u043F\u043E\u0447\u0430\u0442\u043A\
  \u0443 \u0434\u043E\u0434\u0430\u0439\u0442\u0435 \u043F\u0430\u043A\u0435\u0442\
  \ `http` \u0434\u043E \u0432\u0430\u0448\u043E\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:48.797041-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Dart \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043F\
  \u0430\u043A\u0435\u0442 `http` \u0434\u043B\u044F \u043D\u0430\u0434\u0441\u0438\
  \u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432 \u0437\
  \ \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\u0432\u0442\u0435\u043D\u0442\
  \u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E."
title: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F HTTP-\u0437\
  \u0430\u043F\u0438\u0442\u0443 \u0456\u0437 \u0431\u0430\u0437\u043E\u0432\u043E\
  \u044E \u0430\u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\
  \u0454\u044E"
weight: 45
---

## Як зробити:
В Dart ви можете використовувати пакет `http` для надсилання HTTP-запитів з базовою автентифікацією. Спочатку додайте пакет `http` до вашого файлу `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.4
```

Потім імпортуйте пакет у ваш Dart файл:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Для відправлення GET-запиту з базовою автентифікацією, можна використати наступний код:

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
    print('Дані користувача успішно отримані!');
    print('Відповідь тіла: ${response.body}');
  } else {
    print('Не вдалося отримати дані користувача з кодом статусу: ${response.statusCode}');
  }
}
```

Цей код надсилає GET-запит на 'https://yourapi.com/userdata' з заголовком базової автентифікації. Ім'я користувача та пароль кодуються в base64 і передаються в заголовку 'Authorization' згідно зі стандартами базового доступу до автентифікації.

**Приклад виводу:**

У разі успішного запиту та якщо сервер повертає код статусу 200, ви можете побачити:

```plaintext
Дані користувача успішно отримані!
Відповідь тіла: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Якщо автентифікація не вдається або виникає будь-яка інша помилка, код стану відповіді допоможе ідентифікувати проблему.
