---
title:                "Working with CSV"
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, short for Comma-Separated Values, is a ubiquitous file format for storing tabular data. Programmers use it because it's simple, widely supported, and can be easily read and written by both computers and humans.

## How to:

### Reading a CSV File
```php
<?php
$filename = 'data.csv';

if (($handle = fopen($filename, "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        echo "Row: " . print_r($data, true) . "\n";
    }
    fclose($handle);
}
?>
```
Sample Output:
```
Row: Array
(
    [0] => Name
    [1] => Age
    [2] => Email
)

Row: Array
(
    [0] => John Doe
    [1] => 30
    [2] => john@example.com
)
```

### Writing to a CSV File
```php
<?php
$list = [
  ['Name', 'Age', 'Email'],
  ['Jane Doe', '25', 'jane@example.com'],
  ['John Smith', '40', 'john.smith@example.com']
];

$filename = 'output.csv';

$handle = fopen($filename, 'w');

foreach ($list as $fields) {
    fputcsv($handle, $fields);
}

fclose($handle);
?>
```

## Deep Dive
CSV's been around since the early days of computing, making it one of the most enduring data storage formats. While JSON and XML offer more complexity, CSV remains popular for its simplicity. When using PHP to manipulate CSV files, you interact with the file system through built-in functions like `fgetcsv()` and `fputcsv()`. These functions encapsulate the nitty-gritty of file parsing and writing, making it pretty straightforward. Do note that the `fgetcsv()` function allows you to define a length parameter and a delimiter, which you may need to tweak according to your CSV file's specifics.

## See Also
- PHP Official Documentation on fgetcsv: https://www.php.net/manual/en/function.fgetcsv.php
- PHP Official Documentation on fputcsv: https://www.php.net/manual/en/function.fputcsv.php
- Introduction to CSV processing with PHP: https://www.php.net/manual/en/book.fileinfo.php
- Online CSV editor and validator: https://csvlint.io/
- RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files: https://tools.ietf.org/html/rfc4180
