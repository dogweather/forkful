---
title:                "Arbeta med CSV"
aliases:
- /sv/php/working-with-csv/
date:                  2024-02-03T19:21:10.995563-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV (Comma-Separated Values - kommaavgränsade värden) innebär att läsa från och skriva data till CSV-filer, ett populärt format för att representera tabelldata i klartext. Programmerare gör detta för att enkelt utbyta data mellan olika program, system eller databaser, tack vare dess enkelhet och breda stöd på olika plattformar och programmeringsspråk.

## Hur gör man:

PHP erbjuder inbyggda funktioner för att hantera CSV-filer, vilket gör det enkelt att läsa från och skriva till dessa filer utan att behöva tredjepartsbibliotek. Här är exempel för att komma igång:

### Läsa en CSV-fil

Du kan öppna en CSV-fil och läsa dess innehåll med `fopen()` i kombination med `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Antal fält i raden: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Detta skript skriver ut antalet fält i varje rad följt av innehållet i varje fält.

### Skriva till en CSV-fil

För att skriva till en CSV-fil, använd `fopen()` i skrivläge (`w`) och `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Namn', 'E-post'],
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

Detta skript skapar en fil med namnet `users.csv` och skriver rubriken och två rader med data till den.

### Använda ett Bibliotek: League\Csv

För mer avancerad hantering av CSV-filer erbjuder `League\Csv`-biblioteket en robust uppsättning funktioner. Efter att ha installerat det via Composer (`composer require league/csv`), kan du använda det för att läsa och skriva CSV-data mer flexibelt.

#### Läsa med League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Sätt om du vill använda första raden som rubrik

$resultat = $csv->getRecords();
foreach ($resultat as $rad) {
    print_r($rad);
}
?>
```

Detta skript läser `data.csv`, behandlar första raden som kolumnrubriker och skriver ut varje rad som en associativ array.

#### Skriva med League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Namn', 'E-post']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Skrev till users_new.csv framgångsrikt.";
?>
```

Detta skapar `users_new.csv` och skriver en rubrikrad följt av två datarader.
