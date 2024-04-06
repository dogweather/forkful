---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:11.235825-07:00
description: 'Hoe te: Voorbeelduitvoer.'
lastmod: '2024-04-05T21:53:50.940083-06:00'
model: gpt-4-0125-preview
summary: ''
title: Werken met CSV
weight: 37
---

## Hoe te:


### Een CSV-bestand lezen
```php
<?php
$filename = 'data.csv';

if (($handle = fopen($filename, "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        echo "Rij: " . print_r($data, true) . "\n";
    }
    fclose($handle);
}
?>
```
Voorbeelduitvoer:
```
Rij: Array
(
    [0] => Naam
    [1] => Leeftijd
    [2] => E-mail
)

Rij: Array
(
    [0] => John Doe
    [1] => 30
    [2] => john@example.com
)
```

### Naar een CSV-bestand schrijven
```php
<?php
$list = [
  ['Naam', 'Leeftijd', 'E-mail'],
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

## Diep Duiken
CSV is er al sinds de vroege dagen van de informatica, waardoor het een van de meest duurzame gegevensopslagformaten is. Hoewel JSON en XML meer complexiteit bieden, blijft CSV populair vanwege zijn eenvoud. Bij het gebruiken van PHP om CSV-bestanden te manipuleren, interacteer je met het bestandssysteem door middel van ingebouwde functies zoals `fgetcsv()` en `fputcsv()`. Deze functies omvatten de details van bestandsparsing en -schrijven, waardoor het vrij eenvoudig wordt. Let op dat de `fgetcsv()` functie je toestaat om een lengteparameter en een scheidingsteken te definiëren, die je wellicht moet aanpassen aan de specificaties van je CSV-bestand.

## Zie Ook
- Officiële PHP-documentatie over fgetcsv: https://www.php.net/manual/nl/function.fgetcsv.php
- Officiële PHP-documentatie over fputcsv: https://www.php.net/manual/nl/function.fputcsv.php
- Inleiding tot de verwerking van CSV met PHP: https://www.php.net/manual/nl/book.fileinfo.php
- Online CSV-editor en validator: https://csvlint.io/
- RFC 4180, Gemeenschappelijk Formaat en MIME-type voor Comma-Separated Values (CSV) bestanden: https://tools.ietf.org/html/rfc4180
