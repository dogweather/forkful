---
title:                "Робота з CSV"
date:                  2024-02-03T19:21:01.453294-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з CSV (значення, розділені комами) передбачає читання та запис даних у файли CSV, популярний формат для представлення табличних даних у вигляді простого тексту. Програмісти роблять це для того, щоб легко обмінюватися даними між різними програмами, системами або базами даних, завдяки його простоті та широкій підтримці на різних платформах та мовах програмування.

## Як це зробити:

PHP надає вбудовані функції для роботи з файлами CSV, що робить читання та запис у ці файли простим, без потреби в сторонніх бібліотеках. Ось приклади, які допоможуть вам розпочати:

### Читання файлу CSV

Ви можете відкрити файл CSV і прочитати його вміст за допомогою `fopen()` у поєднанні з `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Кількість полів у рядку: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Цей скрипт виводить кількість полів у кожному рядку, за якими слідують вміст кожного поля.

### Запис у файл CSV

Щоб записати в файл CSV, використовуйте `fopen()` в режимі запису (`w`) та `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Ім\'я', 'Електронна пошта'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

Цей скрипт створює файл під назвою `users.csv` і записує заголовок та два рядки даних до нього.

### Використання бібліотеки: League\Csv

Для більш розширеної роботи з CSV, бібліотека `League\Csv` пропонує повний набір можливостей. Після її встановлення через Composer (`composer require league/csv`), ви можете використовувати її для більш гнучкого читання та запису даних CSV.

#### Читання за допомогою League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Використовується, якщо ви хочете використати перший рядок як заголовок

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

Цей скрипт читає `data.csv`, вважаючи перший рядок за заголовки стовпців і виводить кожен рядок як асоційований масив.

#### Запис за допомогою League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Ім\'я', 'Електронна пошта']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Успішно записано у файл users_new.csv.";
?>
```

Це створює `users_new.csv` і записує заголовочний рядок, за яким слідують два рядки даних.
