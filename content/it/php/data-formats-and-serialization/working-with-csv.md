---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:05.523219-07:00
description: "Lavorare con CSV (Comma-Separated Values, ovvero valori separati da\
  \ virgola) comporta la lettura e la scrittura di dati su file CSV, un formato popolare\u2026"
lastmod: '2024-03-13T22:44:43.539191-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con CSV (Comma-Separated Values, ovvero valori separati da virgola)\
  \ comporta la lettura e la scrittura di dati su file CSV, un formato popolare\u2026"
title: Lavorare con i CSV
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con CSV (Comma-Separated Values, ovvero valori separati da virgola) comporta la lettura e la scrittura di dati su file CSV, un formato popolare per rappresentare dati tabellari in testo semplice. I programmatori lo fanno per scambiare dati facilmente tra diversi programmi, sistemi o banche dati, grazie alla sua semplicità e al vasto supporto su piattaforme e linguaggi di programmazione.

## Come fare:

PHP fornisce funzioni integrate per gestire file CSV, rendendo semplice leggere da e scrivere su questi file senza la necessità di librerie di terze parti. Ecco alcuni esempi per iniziare:

### Leggere un File CSV

Puoi aprire un file CSV e leggerne il contenuto usando `fopen()` in combinazione con `fgetcsv()`:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Numero di campi nella riga: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Questo script stampa il numero di campi di ogni riga seguito dal contenuto di ogni campo.

### Scrivere su un File CSV

Per scrivere su un file CSV, usa `fopen()` in modalità scrittura (`w`) e `fputcsv()`:

```php
<?php
$list = [
    ['ID', 'Nome', 'Email'],
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

Questo script crea un file chiamato `users.csv` e scrive l'intestazione e due righe di dati su di esso.

### Uso di una Libreria: League\Csv

Per una gestione CSV più avanzata, la libreria `League\Csv` offre un robusto insieme di funzionalità. Dopo averla installata tramite Composer (`composer require league/csv`), puoi usarla per leggere e scrivere dati CSV con maggiore flessibilità.

#### Leggere con League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Imposta se vuoi usare la prima riga come intestazione

$resultati = $csv->getRecords();
foreach ($resultati as $row) {
    print_r($row);
}
?>
```

Questo script legge `data.csv`, trattando la prima riga come intestazioni di colonna e stampa ogni riga come un array associativo.

#### Scrivere con League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Nome', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Scritto su users_new.csv con successo.";
?>
```

Questo crea `users_new.csv` e scrive una riga di intestazione seguita da due righe di dati.
