---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:58.195730-07:00
description: "Hvordan: PHP tilbyr innebygde funksjoner for h\xE5ndtering av CSV-filer,\
  \ noe som gj\xF8r det enkelt \xE5 lese fra og skrive til disse filene uten \xE5\
  \ m\xE5tte bruke\u2026"
lastmod: '2024-03-13T22:44:40.908624-06:00'
model: gpt-4-0125-preview
summary: "PHP tilbyr innebygde funksjoner for h\xE5ndtering av CSV-filer, noe som\
  \ gj\xF8r det enkelt \xE5 lese fra og skrive til disse filene uten \xE5 m\xE5tte\
  \ bruke tredjepartsbiblioteker."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
PHP tilbyr innebygde funksjoner for håndtering av CSV-filer, noe som gjør det enkelt å lese fra og skrive til disse filene uten å måtte bruke tredjepartsbiblioteker. Her er eksempler for å komme i gang:

### Lese en CSV-fil
Du kan åpne en CSV-fil og lese innholdet ved hjelp av `fopen()` i kombinasjon med `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Antall felt i linjen: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Dette skriptet skriver ut antall felt i hver linje etterfulgt av innholdet i hvert felt.

### Skrive til en CSV-fil
For å skrive til en CSV-fil, bruk `fopen()` i skrivemodus (`w`) og `fputcsv()`:

```php
<?php
$liste = [
    ['ID', 'Navn', 'E-post'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($liste as $rad) {
    fputcsv($handle, $rad);
}

fclose($handle);
?>
```

Dette skriptet oppretter en fil med navnet `users.csv` og skriver overskriften og to rader med data til den.

### Bruke et bibliotek: League\Csv
For mer avansert håndtering av CSV, tilbyr biblioteket `League\Csv` et robust sett med funksjoner. Etter å ha installert det via Composer (`composer require league/csv`), kan du bruke det for å lese og skrive CSV-data mer fleksibelt.

#### Lese med League\Csv
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Sett hvis du vil bruke den første raden som overskrift

$resultater = $csv->getRecords();
foreach ($resultater as $rad) {
    print_r($rad);
}
?>
```

Dette skriptet leser `data.csv`, behandler den første raden som kolonneoverskrifter, og skriver ut hver rad som et assosiativt array.

#### Skrive med League\Csv
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Navn', 'E-post']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Skrevet til users_new.csv med suksess.";
?>
```

Dette oppretter `users_new.csv` og skriver en overskriftsrad etterfulgt av to datarader.
