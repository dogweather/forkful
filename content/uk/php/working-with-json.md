---
title:                "Робота з json"
html_title:           "PHP: Робота з json"
simple_title:         "Робота з json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-json.md"
---

{{< edit_this_page >}}

# Що і чому?

Робота з JSON - це процес обробки даних в форматі JSON (JavaScript Object Notation) за допомогою PHP. Програмісти використовують JSON для збереження та передачі даних, оскільки цей формат є легким, читабельним для людей та машин, а також підтримується багатьма мовами програмування.

# Як:

```php
// Створення масиву даних
$users = array(
  array("name" => "John", "age" => 25, "location" => "Kyiv"),
  array("name" => "Maria", "age" => 30, "location" => "Lviv"),
  array("name" => "Alex", "age" => 28, "location" => "Odessa")
);

// Конвертація у формат JSON
$json = json_encode($users);

// Виведення даних у форматі JSON
echo $json;

// Виведення даних, отриманих з формату JSON
$data = '{"name":"Kate","age":35,"location":"Kharkiv"}';
$user = json_decode($data);
echo $user->name . " is " . $user->age . " years old and lives in " . $user->location;
```

## Deep Dive:

Спочатку JSON був розроблений для JavaScript для зручного обміну даними між браузерами та серверами. Проте згодом став популярним і серед інших мов програмування. Існують такі альтернативи як XML та CSV, але JSON більш поширений та має більш простий синтаксис. У PHP для роботи з JSON використовується функція json_encode() для перетворення даних у формат JSON та json_decode() для отримання даних з формату JSON.

## Дивись також:

Документація про роботу з JSON в PHP: https://www.php.net/manual/en/book.json.php

Офіційний сайт JSON: https://www.json.org/