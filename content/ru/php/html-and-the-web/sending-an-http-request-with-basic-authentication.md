---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:57.933683-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F\
  \ \u0434\u043E\u0431\u0430\u0432\u043B\u0435\u043D\u0438\u0435 \u0438\u043C\u0435\
  \u043D\u0438 \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044F\
  \ \u0438 \u043F\u0430\u0440\u043E\u043B\u044F \u0434\u043B\u044F \u0434\u043E\u0441\
  \u0442\u0443\u043F\u0430 \u043A \u0440\u0435\u0441\u0443\u0440\u0441\u0443 \u043D\
  \u0430 \u0441\u0435\u0440\u0432\u0435\u0440\u0435. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
lastmod: '2024-03-13T22:44:45.209786-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F\
  \ \u0434\u043E\u0431\u0430\u0432\u043B\u0435\u043D\u0438\u0435 \u0438\u043C\u0435\
  \u043D\u0438 \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044F\
  \ \u0438 \u043F\u0430\u0440\u043E\u043B\u044F \u0434\u043B\u044F \u0434\u043E\u0441\
  \u0442\u0443\u043F\u0430 \u043A \u0440\u0435\u0441\u0443\u0440\u0441\u0443 \u043D\
  \u0430 \u0441\u0435\u0440\u0432\u0435\u0440\u0435."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как:
Вот простой способ отправить HTTP-запрос с базовой аутентификацией с использованием cURL в PHP:

```PHP
<?php
$url = 'https://api.example.com/data';
$username = 'your_username';
$password = 'your_password';

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

Пример вывода:

``` 
{
  "authenticated": true,
  "data": "Некоторые безопасные данные"
}
```

## Подробнее
Базовая аутентификация HTTP используется с первых дней существования веба. Это не самый безопасный метод (поскольку учетные данные отправляются в кодировке base64, которую легко декодировать), но его просто реализовать для быстрого и грязного контроля доступа.

Если безопасность вызывает беспокойство (а она должна), вы бы обратились к более надежным методам, таким как OAuth, JWT или ключи API. Тем не менее, базовая аутентификация сохраняется частично из-за устаревших систем, а частично для внутренних систем, где вы строго контролируете доступ.

В PHP для выполнения HTTP-запросов широко используется cURL, но существуют альтернативы, такие как `file_get_contents` или Guzzle (HTTP-клиент для PHP). При использовании `file_get_contents` необходимо создать контекст с соответствующим заголовком:

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$response = file_get_contents($url, false, $context);

echo $response;
?>
```

Выбор подходящего инструмента зависит от потребностей вашего проекта и уровня контроля и функциональности, которые вы желаете.

## Смотрите также
Чтобы углубиться и расширить свои знания, ознакомьтесь с этим:

- [Документация cURL](https://www.php.net/manual/en/book.curl.php)
- [Документация Guzzle](http://docs.guzzlephp.org/en/stable/)
- [Функция PHP `file_get_contents`](https://www.php.net/manual/en/function.file-get-contents.php)
- [Аутентификация через HTTP с помощью PHP](https://www.php.net/manual/en/features.http-auth.php)
