---
title:                "Відправлення HTTP-запиту із базовою автентифікацією"
date:                  2024-03-08T21:57:03.947451-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що та Навіщо?

Надсилання HTTP-запиту з базовою автентифікацією передбачає вказівку імені користувача та пароля до запиту для перевірки особистості користувача. Програмісти використовують це для доступу до ресурсів, що потребують автентифікації, забезпечуючи безпечне зв’язування між клієнтом та сервером.

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
