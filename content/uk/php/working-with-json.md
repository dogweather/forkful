---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке робота з JSON і чому це роблять програмісти? JSON це текстовий формат даних, оптимізований для людського читання та обміну даних між системами. Програмісти використовують JSON через його легкість, стислість та широку підтримку в різних мовах програмування.

## How to:
### Читання JSON
```PHP
$json = '{"name": "Олексій", "age": 30}';
$data = json_decode($json);
echo $data->name; // Виводить: Олексій
```

### Запис JSON
```PHP
$data = ['name' => 'Марія', 'age' => 25];
echo json_encode($data); // Виводить: {"name":"Марія","age":25}
```

### Обробка помилок
```PHP
$json = '{"name": "Андрій", "age": "невідомий"}';
$data = json_decode($json);

if (json_last_error() !== JSON_ERROR_NONE) {
    echo 'Помилка декодування JSON: ' . json_last_error_msg();
} else {
    echo $data->name; // Андрій, якщо немає помилок
}
```

## Deep Dive
JSON (JavaScript Object Notation) з'явився у 2000 році як альтернатива XML. Важливі особливості JSON включають пари ключ-значення і масиви. JSON використовується у RESTful API та багатьох веб-додатках. В PHP, `json_encode()` і `json_decode()` — основні функції для роботи з JSON. Після PHP 5.2.0, JSON став частиною стандартного дистрибутива, а у PHP 7 з'явились значні поліпшення ефективності.

## See Also
- Офіційна документація PHP для роботи з JSON: https://www.php.net/manual/uk/book.json.php
- JSON стандарт: https://www.json.org/json-uk.html
- Уроки з обробки JSON в PHP: https://www.w3schools.com/php/php_ref_json.asp
