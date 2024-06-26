---
date: 2024-01-20 18:02:47.980333-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 PHP, `curl` \u0454 \u043F\u043E\u0442\u0443\u0436\u043D\u0438\u043C \u0441\
  \u043F\u043E\u0441\u043E\u0431\u043E\u043C \u0432\u0456\u0434\u043F\u0440\u0430\u0432\
  \u043B\u0435\u043D\u043D\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432.\
  \ \u041E\u0441\u044C \u044F\u043A \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u0438\
  \u0442\u0438 GET-\u0437\u0430\u043F\u0438\u0442 \u0437 \u0431\u0430\u0437\u043E\u0432\
  \u043E\u044E \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\
  \u0456\u0454\u044E."
lastmod: '2024-03-13T22:44:49.429484-06:00'
model: gpt-4-1106-preview
summary: "\u0423 PHP, `curl` \u0454 \u043F\u043E\u0442\u0443\u0436\u043D\u0438\u043C\
  \ \u0441\u043F\u043E\u0441\u043E\u0431\u043E\u043C \u0432\u0456\u0434\u043F\u0440\
  \u0430\u0432\u043B\u0435\u043D\u043D\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\
  \u0432."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Як це зробити:
У PHP, `curl` є потужним способом відправлення HTTP-запитів. Ось як відправити GET-запит з базовою аутентифікацією:

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, "http://your-protected-resource.com");
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
$status_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);

curl_close($ch);

if ($status_code == 200) {
    echo 'Response: ' . $response;
} else {
    echo 'Error: ' . $status_code;
}
?>
```

Вищенаведений код ініціює сеанс `curl`, налаштовує URL, додає облікові дані для базової аутентифікації, запускає запит, а потім закриває сеанс `curl`. Він також виводить відповідь або код помилки.

## Поглиблений огляд
Базова аутентифікація (Basic Auth) — це метод, за допомогою якого веб-сервери можуть запитувати від користувачів логін і пароль. Вона базується на стандарті HTTP і була одним із перших методів верифікації.

Як альтернативу базовій аутентифікації варто розглянути OAuth і токени доступу, особливо якщо потрібен вищий рівень безпеки.

Реалізація базової аутентифікації в PHP через `curl` - це прямий спосіб маніпулювати HTTP-запитами. `curl_setopt()` використовується для задання параметрів сеансу, в тому числі для включення аутентифікації, встановлення URL-адреси та повернення відповіді. У відповідь, сервер кодує ці облікові дані в `Base64` і передає їх у заголовку `Authorization`.

## Додатково:
- Документація по PHP CURL: https://www.php.net/manual/en/book.curl.php
- Більше про HTTP-аутентифікацію: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- OAuth 2.0: https://oauth.net/2/
- Стаття про безпеку токенів доступу: https://auth0.com/docs/tokens
