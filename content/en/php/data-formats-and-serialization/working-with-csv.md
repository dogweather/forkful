---
date: 2024-02-03 19:03:26.357603-07:00
description: "Working with CSV (Comma-Separated Values) involves reading from and\
  \ writing data to CSV files, a popular format for representing tabular data in plain\u2026"
lastmod: '2024-02-25T18:49:56.626745-07:00'
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) involves reading from and writing\
  \ data to CSV files, a popular format for representing tabular data in plain\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) involves reading from and writing data to CSV files, a popular format for representing tabular data in plain text. Programmers do it to easily exchange data between different programs, systems, or databases, thanks to its simplicity and wide support across platforms and programming languages.

## How to:

PHP provides built-in functions for handling CSV files, making it straightforward to read from and write to these files without needing third-party libraries. Here are examples to get you started:

### Reading a CSV File

You can open a CSV file and read its contents using `fopen()` in combination with `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Number of fields in line: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

This script prints each line's number of fields followed by the content of each field.

### Writing to a CSV File

To write to a CSV file, use `fopen()` in write mode (`w`) and `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Name', 'Email'],
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

This script creates a file named `users.csv` and writes the header and two rows of data to it.

### Using a Library: League\Csv

For more advanced CSV handling, the `League\Csv` library offers a robust set of features. After installing it via Composer (`composer require league/csv`), you can use it to read and write CSV data more flexibly.

#### Reading with League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Set if you want to use the first row as header

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

This script reads `data.csv`, treating the first row as column headers and prints each row as an associative array.

#### Writing with League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Name', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Written to users_new.csv successfully.";
?>
```

This creates `users_new.csv` and writes a header row followed by two data rows.
