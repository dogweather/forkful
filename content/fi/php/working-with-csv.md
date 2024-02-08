---
title:                "Työskentely CSV:n kanssa"
aliases:
- fi/php/working-with-csv.md
date:                  2024-02-03T19:21:15.706053-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely CSV:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Työskentely CSV:n (pilkuilla erotetut arvot) parissa käsittää datan lukemista ja kirjoittamista CSV-tiedostoihin, jotka ovat suosittu formaatti välittämään taulukkomuotoista dataa pelkässä tekstissä. Ohjelmoijat tekevät niin helpottaakseen datan vaihtoa eri ohjelmien, järjestelmien tai tietokantojen välillä, kiitos formaatin yksinkertaisuuden ja laajan tuen alustoilla ja ohjelmointikielillä.

## Kuinka:

PHP tarjoaa sisäänrakennettuja funktioita CSV-tiedostojen käsittelyyn, tekee lukemisen ja kirjoittamisen näihin tiedostoihin suoraviivaiseksi ilman kolmannen osapuolen kirjastoja tarvita. Tässä on esimerkkejä, joiden avulla pääset alkuun:

### CSV-tiedoston lukeminen

Voit avata CSV-tiedoston ja lukea sen sisällön käyttäen `fopen()` yhdessä `fgetcsv()` kanssa:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Kenttien lukumäärä rivillä: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Tämä skripti tulostaa jokaisen rivin kenttien määrän seurattuna jokaisen kentän sisällöllä.

### Kirjoittaminen CSV-tiedostoon

CSV-tiedostoon kirjoittamiseen, käytä `fopen()` kirjoitusmoodissa (`w`) ja `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Nimi', 'Sähköposti'],
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

Tämä skripti luo tiedoston nimeltä `users.csv` ja kirjoittaa otsikon ja kaksi datariviä siihen.

### Kirjaston käyttö: League\Csv

Monimutkaisempaan CSV-käsittelyyn, `League\Csv` kirjasto tarjoaa vankan ominaisuusjoukon. Asentamalla sen Composerin kautta (`composer require league/csv`), voit käyttää sitä lukemaan ja kirjoittamaan CSV-dataa joustavammin.

#### Lukeminen League\Csv:llä

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Aseta, jos haluat käyttää ensimmäistä riviä otsikkona

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

Tämä skripti lukee `data.csv`:n, käsittelee ensimmäisen rivin sarakkeiden otsikkoina ja tulostaa jokaisen rivin assosiatiivisena taulukkona.

#### Kirjoittaminen League\Csv:llä

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Nimi', 'Sähköposti']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Kirjoitettu onnistuneesti users_new.csv.";
?>
```

Tämä luo `users_new.csv` ja kirjoittaa otsikkorivin seurattuna kahdella datarivillä.
