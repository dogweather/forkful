---
title:                "PHP: Робота з json"
simple_title:         "Робота з json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Структурований формат JSON є важливим інструментом для роботи з даними в PHP. Цей формат дозволяє ефективно передавати та зберігати дані у веб-додатках.

## Як працювати з JSON в PHP

Найпростіший спосіб роботи з JSON в PHP - це встановлення функцій `json_encode()` та `json_decode()`.

```PHP
// Кодування масива в JSON рядок
$array = ["name" => "Ivan", "age" => 25];

echo json_encode($array); // виведе {"name": "Ivan", "age": 25}

// Декодування JSON рядка у масив
$json = '{"name": "Maria", "age": 30}';

var_dump(json_decode($json)); // виведе: array(2) { ["name"]=> string(5) "Maria", ["age"]=> int(30) }
```

Методи `json_encode()` та `json_decode()` також приймають додаткові параметри для налаштування форматування та обробки даних. Детальніше про ці методи можна дізнатися в [офіційній документації PHP](https://www.php.net/manual/en/book.json.php).

## Глибокий занурення

JSON має просту структуру з ключ-значення парами, що робить його досить зручним для перетворення даних між різними мовами програмування. Крім того, в PHP також існує багато функцій для обробки різних типів даних, які можна легко поєднати зі стандартними функціями JSON.

Наприклад, функція `in_array()` дозволяє швидко перевірити чи є певний елемент у масиві, що отриманий після декодування JSON:

```PHP
$json = '[{"name": "Peter", "age": 35}, {"name": "Anna", "age": 27}, {"name": "Mark", "age": 40}]';

$array = json_decode($json);

if(in_array("Anna", array_column($array, "name"))){ // перевіряємо чи присутня "Anna" серед значень у ключі "name"
  echo "В масиві є Anna!";
}

// виведе: В масиві є Anna!
```

Зверніть увагу на використання функції `array_column()`, яка дозволяє отримати значення певного ключа з масиву з масивами.

## Дивись також

[Офіційна документація PHP для роботи з JSON](https://www.php.net/manual/en/book.json.php)

[Ukrainian PHP Community](https://ua.php.net/)

[Ukrainian Programming Language Lovers](https://www.facebook.com/groups/ukraine.lang/)