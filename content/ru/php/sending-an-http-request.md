---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:33.285478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса — это процесс, посредством которого программа запрашивает данные с сервера. Программисты делают это для взаимодействия с веб-сервисами, API или просто для получения содержимого веб-страниц.

## Как это сделать:

PHP имеет отличный способ обработки HTTP-запросов с помощью библиотеки `cURL`. Но новинка — использование `file_get_contents` для более простых GET-запросов, или `stream_context_create` для POST-запросов. Вот быстрый взгляд на оба метода.

### GET-запрос с file_get_contents():
```php
// URL, который вы используете
$url = "http://example.com/api";

// Используйте file_get_contents для выполнения GET-запроса
$response = file_get_contents($url);

// Выведите результат, чтобы увидеть, что вы получили
var_dump($response);
```

### POST-запрос с stream_context_create():
```php
// URL, на который вы отправляете запрос
$url = "http://example.com/api";

// Данные, которые вы отправляете
$data = http_build_query([
    'foo' => 'bar',
    'baz' => 'qux',
]);

// Опции контекста потока
$options = [
    'http' => [
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => $data,
    ],
];

// Создайте контекст потока
$context  = stream_context_create($options);

// Выполните POST-запрос и поместите ответ в переменную
$result = file_get_contents($url, false, $context);

// Посмотрите, что вы получили
var_dump($result);
```

## Глубокое погружение

Раньше `fsockopen()` был основным инструментом для HTTP-запросов в PHP. Это было неуклюже, но работало. Потом появился `cURL`, который по-прежнему мощный и широко используется, особенно для сложных операций. Но иногда вам не нужна бензопила, чтобы разрезать кусок веревки. Вот где отлично подходят `file_get_contents()` и `stream_context_create()`.

Одна ключевая особенность `file_get_contents()` — это простота. Идеально подходит для простых GET-запросов. Но что, если вам нужно отправить данные методом POST? Здесь на помощь приходит `stream_context_create()`. Этот маленький драгоценный камень позволяет вам тонко настроить ваши HTTP-запросы с помощью заголовков, методов и многого другого.

Внутри `file_get_contents()` и `stream_context_create()` используют оболочки потоков PHP. Они заменяют операции с сокетами низкого уровня, которые обрабатываются через `fsockopen()`.

Однако есть минус? Обработка ошибок может быть усложнена. Если что-то идет не так, эти функции менее снисходительны, чем `cURL`. Если вам нужна подробная информация о ответе или вы имеете дело со сложными HTTP-задачами, рассмотрите возможность оставаться с `cURL`.

## Смотрите также

- Официальная документация PHP по cURL: [https://www.php.net/manual/ru/book.curl.php](https://www.php.net/manual/ru/book.curl.php)
- Контексты потоков PHP: [https://www.php.net/manual/ru/context.php](https://www.php.net/manual/ru/context.php)
- Опции контекста HTTP: [https://www.php.net/manual/ru/context.http.php](https://www.php.net/manual/ru/context.http.php)
