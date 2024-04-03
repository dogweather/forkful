---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:33.285478-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: PHP \u0438\u043C\u0435\u0435\u0442 \u043E\u0442\u043B\u0438\u0447\u043D\
  \u044B\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u043E\u0431\u0440\u0430\u0431\
  \u043E\u0442\u043A\u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 `cURL`. \u041D\u043E \u043D\u043E\u0432\u0438\u043D\u043A\
  \u0430 \u2014 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435 `file_get_contents` \u0434\u043B\u044F \u0431\u043E\u043B\u0435\u0435\
  \u2026"
lastmod: '2024-03-13T22:44:45.204406-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0438\u043C\u0435\u0435\u0442 \u043E\u0442\u043B\u0438\u0447\u043D\u044B\
  \u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u043E\u0431\u0440\u0430\u0431\u043E\
  \u0442\u043A\u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 `cURL`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

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
