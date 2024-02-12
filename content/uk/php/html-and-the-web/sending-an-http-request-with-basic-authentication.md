---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases: - /uk/php/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:47.980333-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Відправлення HTTP-запиту з базовою аутентифікацією дозволяє вам передавати ім'я користувача та пароль для доступу до захищеного ресурсу. Програмісти роблять це, щоб забезпечити безпечний обмін даними між клієнтом та сервером.

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
