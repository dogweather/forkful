---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:39.841012-07:00
description: ''
lastmod: '2024-04-05T22:50:58.691582-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как это сделать:


### Чтение файла CSV
```php
<?php
$filename = 'data.csv';

if (($handle = fopen($filename, "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        echo "Строка: " . print_r($data, true) . "\n";
    }
    fclose($handle);
}
?>
```
Пример вывода:
```
Строка: Массив
(
    [0] => Имя
    [1] => Возраст
    [2] => Электронная почта
)

Строка: Массив
(
    [0] => Джон Доу
    [1] => 30
    [2] => john@example.com
)
```

### Запись в файл CSV
```php
<?php
$list = [
  ['Имя', 'Возраст', 'Электронная почта'],
  ['Джейн Доу', '25', 'jane@example.com'],
  ['Джон Смит', '40', 'john.smith@example.com']
];

$filename = 'output.csv';

$handle = fopen($filename, 'w');

foreach ($list as $fields) {
    fputcsv($handle, $fields);
}

fclose($handle);
?>
```

## Глубокое погружение
CSV существует с первых дней вычислительной техники, что делает его одним из самых долговечных форматов хранения данных. Несмотря на то что форматы JSON и XML предлагают большую сложность, CSV остаётся популярным за его простоту. Используя PHP для работы с файлами CSV, вы взаимодействуете с файловой системой через встроенные функции, такие как `fgetcsv()` и `fputcsv()`. Эти функции инкапсулируют все тонкости парсинга и записи файлов, делая процесс достаточно простым. Обратите внимание, что функция `fgetcsv()` позволяет определить параметр длины и разделитель, которые вам, возможно, придётся настраивать в соответствии со спецификациями вашего файла CSV.

## См. также
- Официальная документация PHP по функции fgetcsv: https://www.php.net/manual/ru/function.fgetcsv.php
- Официальная документация PHP по функции fputcsv: https://www.php.net/manual/ru/function.fputcsv.php
- Введение в обработку CSV с помощью PHP: https://www.php.net/manual/ru/book.fileinfo.php
- Онлайн-редактор и валидатор CSV: https://csvlint.io/
- RFC 4180, Общий формат и MIME-тип для файлов с разделёнными запятыми значениями (CSV): https://tools.ietf.org/html/rfc4180
