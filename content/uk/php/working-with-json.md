---
title:                "Робота з JSON"
aliases:
- uk/php/working-with-json.md
date:                  2024-02-03T19:23:55.012637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
