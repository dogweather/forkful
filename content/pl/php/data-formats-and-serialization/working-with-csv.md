---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:45.548710-07:00
description: "Praca z CSV (Comma-Separated Values - warto\u015Bciami rozdzielonymi\
  \ przecinkami) polega na odczycie z i zapisie danych do plik\xF3w CSV, popularnego\
  \ formatu\u2026"
lastmod: 2024-02-19 22:04:54.655496
model: gpt-4-0125-preview
summary: "Praca z CSV (Comma-Separated Values - warto\u015Bciami rozdzielonymi przecinkami)\
  \ polega na odczycie z i zapisie danych do plik\xF3w CSV, popularnego formatu\u2026"
title: Praca z plikami CSV
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z CSV (Comma-Separated Values - wartościami rozdzielonymi przecinkami) polega na odczycie z i zapisie danych do plików CSV, popularnego formatu reprezentacji danych tabelarycznych w postaci zwykłego tekstu. Programiści robią to, aby łatwo wymieniać dane między różnymi programami, systemami lub bazami danych, dzięki jego prostocie i szerokiemu wsparciu na różnych platformach i w językach programowania.

## Jak to zrobić:

PHP oferuje wbudowane funkcje do obsługi plików CSV, co sprawia, że odczyt z nich i zapis do nich jest prosty bez potrzeby używania bibliotek stron trzecich. Oto przykłady, które pomogą Ci zacząć:

### Odczytywanie pliku CSV

Możesz otworzyć plik CSV i odczytać jego zawartość za pomocą `fopen()` w połączeniu z `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Liczba pól w linii: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Ten skrypt wypisuje liczbę pól każdej linii, a następnie zawartość każdego pola.

### Zapisywanie do pliku CSV

Aby zapisać do pliku CSV, użyj `fopen()` w trybie zapisu (`w`) i `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Nazwa', 'Email'],
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

Ten skrypt tworzy plik o nazwie `users.csv` i zapisuje do niego nagłówek i dwie linie danych.

### Użycie biblioteki: League\Csv

Dla bardziej zaawansowanej obsługi CSV, biblioteka `League\Csv` oferuje solidny zestaw funkcji. Po jej instalacji poprzez Composer (`composer require league/csv`), możesz jej użyć do bardziej elastycznego czytania i zapisywania danych CSV.

#### Odczytywanie za pomocą League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Ustaw, jeśli chcesz używać pierwszego wiersza jako nagłówek

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

Ten skrypt czyta `data.csv`, traktując pierwszy wiersz jako nagłówki kolumn i wypisuje każdy wiersz jako asocjacyjną tablicę.

#### Zapisywanie za pomocą League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Nazwa', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Zapisano do users_new.csv pomyślnie.";
?>
```

To tworzy `users_new.csv` i zapisuje wiersz nagłówkowy, a następnie dwa wiersze danych.
