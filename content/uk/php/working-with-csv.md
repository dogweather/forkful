---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Що та чому?

Робота з CSV (Comma-Separated Values) - обробка текстових файлів, що містять дані, розділені комами. Програмісти використовують CSV через його простоту і універсальність для обміну даними між різними програмами і системами.

## Как це зробити:

Обробка CSV в PHP проста. Щоб читати CSV-файл використовуємо `fgetcsv`, для створення - `fputcsv`.

Читання CSV файлу:
```php
<?php
$filename = 'data.csv';
if (($h = fopen("{$filename}", "r")) !== FALSE) {
    while (($data = fgetcsv($h, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($h);
}
?>
```

Створення CSV файлу:
```php
<?php
$list = array (
  array('Name', 'Email', 'Phone'),
  array('John Doe', 'john@example.com', '1234567890')
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```

## Поглиблений аналіз:

CSV стандарт не регламентований, що веде до різниці в обробці файлів. Альтернативами є JSON або XML, які мають чіткісінтаксис, але CSV залишається популярним через свою простоту. При реалізації важливо враховувати можливість виникнення розбіжностей у кодуванні символів (наприклад, UTF-8 vs Windows-1251).

## Додатково:

Для більш детального ознайомлення з CSV в PHP:
- PHP Manual по роботі з файлами: https://www.php.net/manual/en/book.filesystem.php
- PHP Manual по функціям `fgetcsv` і `fputcsv`: https://www.php.net/manual/en/function.fgetcsv.php, https://www.php.net/manual/en/function.fputcsv.php
- Розгляд популярних бібліотек для роботи з CSV: https://github.com/thephpleague/csv
