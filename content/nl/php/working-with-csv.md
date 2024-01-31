---
title:                "Werken met CSV"
date:                  2024-01-28T22:10:11.235825-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
CSV, de afkorting voor Comma-Separated Values (komma-gescheiden waarden), is een alomtegenwoordig bestandsformaat voor het opslaan van tabelgegevens. Programmeurs gebruiken het omdat het eenvoudig is, breed ondersteund wordt, en gemakkelijk te lezen en te schrijven is door zowel computers als mensen.

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
