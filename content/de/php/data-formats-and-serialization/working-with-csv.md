---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:32.250924-07:00
description: "Wie: PHP bietet eingebaute Funktionen zur Handhabung von CSV-Dateien,\
  \ wodurch es unkompliziert wird, aus diesen Dateien zu lesen und in sie zu schreiben,\u2026"
lastmod: '2024-03-13T22:44:53.995365-06:00'
model: gpt-4-0125-preview
summary: "PHP bietet eingebaute Funktionen zur Handhabung von CSV-Dateien, wodurch\
  \ es unkompliziert wird, aus diesen Dateien zu lesen und in sie zu schreiben, ohne\
  \ dass Drittanbieterbibliotheken ben\xF6tigt werden."
title: Arbeiten mit CSV
weight: 37
---

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
