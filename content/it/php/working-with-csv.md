---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV sta per "Comma-Separated Values". Programatori usano formati CSV per trasportare dati: è semplice, universale, e leggibile da umani e macchine.

## How to:
Leggi CSV:
```PHP
<?php
if (($handle = fopen("esempio.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($handle);
}
?>
```
Salva CSV:
```PHP
<?php
$list = array (
    array('ciao', 'mondo', '2023'),
    array('php', 'csv', 'esempio')
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```
Output di lettura potrebbe essere:
```
Array
(
    [0] => ciao
    [1] => mondo
    [2] => 2023
)
Array
(
    [0] => php
    [1] => csv
    [2] => esempio
)
```

## Deep Dive
CSV nasce nei primi anni '70. Alternative includono JSON e XML, ma CSV brilla per la sua semplicità. Quando lavori con CSV in PHP, controlla il locale (funzione `setlocale()`) perché influisce sulla separazione dei decimali.

## See Also
- Documentazione PHP su fgetcsv: https://www.php.net/manual/it/function.fgetcsv.php
- Documentazione PHP su fputcsv: https://www.php.net/manual/it/function.fputcsv.php
- RFC 4180, il standard del formato CSV: https://tools.ietf.org/html/rfc4180
