---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z CSV (Comma Separated Values) umożliwia obsługę prostych tabel danych – zarówno ich odczyt, jak i zapis. Programiści korzystają z formatu CSV ze względu na jego uniwersalność i prostotę, która gwarantuje łatwą wymianę danych między różnymi systemami i aplikacjami.

## Jak to zrobić:
Oto kilka przykładów kodu PHP, które pokazują, jak obsłużyć pliki CSV:

Odczyt z pliku CSV:
```PHP
<?php
$plik = fopen('dane.csv', 'r');
while (($dane = fgetcsv($plik, 1000, ",")) !== FALSE) {
    $num = count($dane);
    for ($c=0; $c < $num; $c++) {
        echo $dane[$c] . "\n";
    }
}
fclose($plik);
?>
```
Zapis do pliku CSV:
```PHP
<?php
$list = array(
    array('Jan', 'Kowalski', 'jan@example.com'),
    array('Anna', 'Nowak', 'anna@example.com'),
);

$plik = fopen('dane.csv', 'w');
foreach ($list as $linia) {
    fputcsv($plik, $linia);
}
fclose($plik);
?>
```

## W głębi tematu:
Format CSV powstał w pierwszych latach komputerów. Jego prostota sprawiła, że szybko stał się popularny w wymianie danych. Alternatywami dla CSV są m.in. JSON, XML, czy bazy danych, ale CSV nadal jest używany ze względu na prostotę i czytelność. Przy pracy z CSV ważne jest rozważenie zakodowania znaków (na przykład UTF-8) oraz obsługi różnych separatorów i cudzysłowów, co może być kluczowe dla poprawnego przetwarzania danych.

## Zobacz też:
- Dokumentacja PHP na temat funkcji obsługi CSV: [php.net/manual/en/function.fgetcsv.php](https://www.php.net/manual/en/function.fgetcsv.php)
- W3Schools tutorial na temat pracy z plikami w PHP: [w3schools.com/php/php_file.asp](https://www.w3schools.com/php/php_file.asp)
- Stack Overflow - wspólnota programistów z dyskusjami na temat specyficznych problemów przy pracy z CSV: [stackoverflow.com/questions/tagged/csv?tab=Votes](https://stackoverflow.com/questions/tagged/csv?tab=Votes)
