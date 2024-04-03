---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:55.012637-07:00
description: "JSON, \u0430\u0431\u043E JavaScript Object Notation, \u0446\u0435 \u043B\
  \u0435\u0433\u043A\u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u044F\u043A\u0438\
  \u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\u0438 \u0442\
  \u0430 \u043F\u0438\u0441\u0430\u0442\u0438 \u043B\u044E\u0434\u044F\u043C, \u0430\
  \ \u0442\u0430\u043A\u043E\u0436 \u043B\u0435\u0433\u043A\u043E \u043E\u0431\u0440\
  \u043E\u0431\u043B\u044F\u0442\u0438 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0443\u0432\u0430\u0442\u0438 \u043C\u0430\u0448\u0438\u043D\u0430\u043C\u0438\
  .\u2026"
lastmod: '2024-03-13T22:44:49.467179-06:00'
model: gpt-4-0125-preview
summary: "JSON, \u0430\u0431\u043E JavaScript Object Notation, \u0446\u0435 \u043B\
  \u0435\u0433\u043A\u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u044F\u043A\u0438\
  \u0439 \u043B\u0435\u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0442\u0438 \u0442\
  \u0430 \u043F\u0438\u0441\u0430\u0442\u0438 \u043B\u044E\u0434\u044F\u043C, \u0430\
  \ \u0442\u0430\u043A\u043E\u0436 \u043B\u0435\u0433\u043A\u043E \u043E\u0431\u0440\
  \u043E\u0431\u043B\u044F\u0442\u0438 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0443\u0432\u0430\u0442\u0438 \u043C\u0430\u0448\u0438\u043D\u0430\u043C\u0438."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Що і чому?
JSON, або JavaScript Object Notation, це легкий формат обміну даними, який легко читати та писати людям, а також легко обробляти та генерувати машинами. Програмісти часто працюють з JSON для обміну даними між серверами та веб-додатками через його простоту та незалежність від мови, що робить його краєугольним каменем сучасної веб-розробки та API.

## Як це робиться:
Робота з JSON у PHP є простою завдяки вбудованим функціям `json_encode()` та `json_decode()`. Нижче наведено приклади, що демонструють як конвертувати PHP масив у JSON рядок, та навпаки:

### Кодування PHP масиву в JSON рядок
```php
// Визначаємо асоціативний масив
$data = [
    "name" => "Джон Доу",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// Конвертуємо PHP масив в JSON рядок
$jsonString = json_encode($data);

// Виводимо JSON рядок
echo $jsonString;
```
**Приклад виводу:**
```json
{"name":"Джон Доу","age":30,"email":"john.doe@example.com"}
```

### Декодування JSON рядка в PHP масив
```php
// JSON рядок
$jsonString = '{"name":"Джон Доу","age":30,"email":"john.doe@example.com"}';

// Конвертуємо JSON рядок в PHP масив
$data = json_decode($jsonString, true);

// Виводимо PHP масив
print_r($data);
```
**Приклад виводу:**
```
Array
(
    [name] => Джон Доу
    [age] => 30
    [email] => john.doe@example.com
)
```

### Робота з сторонньою бібліотекою: GuzzleHttp
Для складнішої обробки JSON та веб-запитів однією з популярних PHP бібліотек є GuzzleHttp. Вона спрощує HTTP-запити та легко працює з даними у форматі JSON.

**Встановлення через Composer:**
```
composer require guzzlehttp/guzzle
```

**Приклад запиту:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Відправлення запиту до API, що повертає JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Декодування JSON відповіді в PHP масив
$data = json_decode($response->getBody(), true);

// Вивід даних
print_r($data);
```

**Припускаючи, що API повертає схожі JSON дані:**
```
Array
(
    [name] => Джон Доу
    [age] => 30
    [email] => john.doe@example.com
)
```
Це демонструє легкість використання PHP для маніпуляцій з JSON, як з рідними функціями, так і з потужними бібліотеками на зразок GuzzleHttp для складніших завдань.
