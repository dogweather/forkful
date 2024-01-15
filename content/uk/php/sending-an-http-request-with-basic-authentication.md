---
title:                "Надіслання запиту http з базовою автентифікацією"
html_title:           "PHP: Надіслання запиту http з базовою автентифікацією"
simple_title:         "Надіслання запиту http з базовою автентифікацією"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запиту зі звичайною аутентифікацією є зручним і безпечним шляхом з'єднання з веб-сайтами та іншими серверами, які вимагають підтвердження для доступу до своїх ресурсів.

## Як

```PHP
<?php
$ch = curl_init("https://www.example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "{username}:{password}");
$output = curl_exec($ch);
curl_close($ch);
```

При відправленні HTTP-запиту з використанням PHP, можна вказати тип аутентифікації через опцію `CURLOPT_HTTPAUTH` і вказати користувача та пароль через опцію `CURLOPT_USERPWD`. Результатом буде вивід вмісту сторінки "www.example.com".

## Заглибившися

Виглядає просто, але є кілька нюансів, про які потрібно пам'ятати при використанні HTTP-запитів зі звичайною аутентифікацією. При використанні `CURLAUTH_BASIC`, дані автентифікації будуть передані у форматі base64, що може бути вразливим до атак на перехоплення. Тому рекомендується використовувати HTTPS замість звичного HTTP для забезпечення безпеки.

## Дивись також

- [cURL Documentation](https://www.php.net/manual/en/curl.examples-basic.php)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)