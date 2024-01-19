---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?
Відправлення HTTP-запиту з базовою аутентифікацією - це процес подання імені користувача і пароля в заголовках HTTP для отримання доступу до захищених ресурсів. Програмісти це роблять, щоб забезпечити безпеку та контроль доступу до своїх веб-служб або API.

## Зробити так:
Перевірте наступний код, що демонструє, як відправити HTTP-запит з базовою аутентифікацією в PHP:

```PHP
<?php
$url = 'https://your-api-url.com';
$username = 'your-username';
$password = 'your-password';

$context = stream_context_create(array(
    'http' => array(
        'header'  => "Authorization: Basic " . base64_encode("$username:$password")
    )
));
$response = file_get_contents($url, false, $context);

if ($response) {
    echo $response;
} else {
    echo "HTTP request failed!";
}
?>
```
У виводі ви побачите відповідь сервера або повідомлення про помилку, якщо запит не вдався.

## На глибше:
Як історичний контекст, основна аутентифікація HTTP була однією з перших методів захисту веб-ресурсів, використану в HTTP/1.0 з 1996 року. Інші альтернативи включають Digest Access Authentication, OAuth, та JWT, які надають більший рівень безпеки.

Під час відправлення HTTP-запиту з основною аутентифікацією, ім'я користувача та пароль, кодовані в Base64, вставляються в заголовок "Authorization", який відправляється до сервера. 

## Додатково:
1. [Основна аутентифікація HTTP (MDN)](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
2. [HTTP-аутентифікація в PHP (PHP.net)](https://www.php.net/manual/en/features.http-auth.php)
3. [Алтернативи основної аутентифікації (authentication.com)](https://authentication.com/alternative-authentication-methods-to-basic-authentication)