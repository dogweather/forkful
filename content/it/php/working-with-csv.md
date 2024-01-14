---
title:                "PHP: Utilizzare i file csv"
simple_title:         "Utilizzare i file csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
CSV (Comma-Separated Values) è un formato di file comunemente utilizzato per lo scambio di dati tra diverse applicazioni e database. Sapere come lavorare con i file CSV può semplificare notevolmente il processo di importazione ed esportazione dei dati in un progetto di programmazione PHP.

## Come fare
Il seguente esempio di codice mostra come aprire e leggere un file CSV utilizzando la funzione `fopen` e il metodo `fgetcsv` in PHP. Sarà quindi stampato il contenuto del file CSV a schermo.

```PHP
<?php

// Apriamo il file CSV in modalità lettura
$file = fopen('file.csv', 'r');

// Leggiamo il contenuto del file riga per riga
while($data = fgetcsv($file)) {
    // Stampa i dati della riga in un formato leggibile
    print_r($data);
}

// Chiudiamo il file
fclose($file);

// Output:
// Array
// (
//     [0] => ID
//     [1] => Nome
//     [2] => Cognome
// )
// Array
// (
//     [0] => 1
//     [1] => Marco
//     [2] => Rossi
// )
// Array
// (
//     [0] => 2
//     [1] => Laura
//     [2] => Bianchi
// )
```

## Approfondimento
Il formato di file CSV è molto flessibile e offre molte opzioni per la gestione dei dati. Ad esempio, è possibile specificare un delimitatore diverso dalla virgola o definire un carattere di quote per i dati. Inoltre, PHP offre funzioni come `fputcsv` per scrivere dati in un file CSV e `str_getcsv` per leggere dati da una stringa CSV.

## Vedi anche
- [Documentazione PHP su file CSV](https://www.php.net/manual/en/function.fgetcsv.php)
- [Tutorial su come lavorare con file CSV in PHP](https://www.codementor.io/@developmentworkz/how-to-read-csv-file-in-php-and-display-data-on-html-page-y42rj2vaj)