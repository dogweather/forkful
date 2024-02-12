---
title:                "Arbeiten mit CSV"
aliases:
- /de/php/working-with-csv/
date:                  2024-02-03T19:20:32.250924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV-Dateien (Comma-Separated Values, auf Deutsch etwa "durch Kommas getrennte Werte") umfasst das Lesen von und das Schreiben in CSV-Dateien, einem beliebten Format zur Darstellung von tabellarischen Daten in Klartext. Programmierer tun dies, um Daten zwischen verschiedenen Programmen, Systemen oder Datenbanken leicht austauschen zu können, dank seiner Einfachheit und breiten Unterstützung über Plattformen und Programmiersprachen hinweg.

## Wie:

PHP bietet eingebaute Funktionen zur Handhabung von CSV-Dateien, wodurch es unkompliziert wird, aus diesen Dateien zu lesen und in sie zu schreiben, ohne dass Drittanbieterbibliotheken benötigt werden. Hier sind Beispiele, um Ihnen den Einstieg zu erleichtern:

### Lesen einer CSV-Datei

Sie können eine CSV-Datei öffnen und ihren Inhalt mittels `fopen()` in Kombination mit `fgetcsv()` lesen:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Anzahl der Felder in Zeile: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Dieses Skript druckt die Anzahl der Felder in jeder Zeile gefolgt vom Inhalt jedes Feldes.

### Schreiben in eine CSV-Datei

Um in eine CSV-Datei zu schreiben, verwenden Sie `fopen()` im Schreibmodus (`w`) und `fputcsv()`:

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

Dieses Skript erstellt eine Datei namens `users.csv` und schreibt die Kopfzeile sowie zwei Datensätze in sie.

### Verwendung einer Bibliothek: League\Csv

Für eine fortgeschrittenere Handhabung von CSV-Daten bietet die Bibliothek `League\Csv` einen robusten Satz von Funktionen. Nach der Installation über Composer (`composer require league/csv`) können Sie sie flexibler zum Lesen und Schreiben von CSV-Daten verwenden.

#### Lesen mit League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Setzen Sie dies, wenn Sie die erste Zeile als Kopfzeile verwenden möchten

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

Dieses Skript liest `data.csv`, behandelt die erste Zeile als Spaltenüberschriften und gibt jede Zeile als assoziatives Array aus.

#### Schreiben mit League\Csv

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

echo "Erfolgreich in users_new.csv geschrieben.";
?>
```

Das erstellt `users_new.csv` und schreibt eine Kopfzeile gefolgt von zwei Datenreihen.
