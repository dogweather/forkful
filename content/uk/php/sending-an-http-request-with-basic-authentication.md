---
title:                "Відправлення http-запиту з базовою аутентифікацією"
html_title:           "PHP: Відправлення http-запиту з базовою аутентифікацією"
simple_title:         "Відправлення http-запиту з базовою аутентифікацією"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Відправлення HTTP-запиту з основною автентифікацією - це процес, при якому програміст використовує HTTP-протокол для взаємодії з веб-сервером та передачі інформації. Цей метод часто використовується для звернення до захищеного ресурсу, де потрібно підтвердження ідентифікації користувача.

## Як це зробити:

```php
// Встановлюємо заголовки для базової автентифікації
$username = 'username';
$password = 'password';
$headers = ['Authorization: Basic ' . base64_encode("$username:$password")];

// Відправляємо HTTP-запит до веб-сервера
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'http://example.com');
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$output = curl_exec($ch);
curl_close($ch);

// Виводимо отриманий результат
echo $output;
```

## Глибоке занурення:

- Історичний контекст: HTTP побудований на протоколі створенному в 1995 році, що вимагало визначення механізмів багатократної автентифікації, таких як базова автентифікація.
- Альтернативи: Окрім базової автентифікації, існують інші методи автентифікації, такі як OAuth та JWT.
- Деталі реалізації: Для надійного захисту даних, важливо користуватися HTTPS транспортним рівнем для зашифрування інформації передаваної через HTTP-запит.

## Дивіться також:

- [MDN - Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [PHP Manual - curl_setopt](https://www.php.net/manual/en/function.curl-setopt.php)
- [OAuth 2.0 vs. JWT: Корисне відео](https://www.youtube.com/watch?v=bswxshgGMQo)