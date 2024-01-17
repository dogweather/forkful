---
title:                "Lavorare con i file csv"
html_title:           "PHP: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con CSV, o file di valore separati da virgole, è un modo comune per gestire grandi quantità di dati tabulari in PHP e altri linguaggi di programmazione. I programmatori spesso utilizzano il formato CSV per importare o esportare i dati tra diverse applicazioni e sistemi.

## Come fare:

Per iniziare, è possibile utilizzare la funzione PHP integrata ```fgetcsv()``` per leggere i dati da un file CSV e memorizzarli in una matrice. Di seguito è riportato un esempio di codice che mostra come ottenere i dati da un file CSV e stampare il contenuto in una tabella HTML:

```PHP
<?php
$csv_filename = "dati.csv";
$handle = fopen($csv_filename, "r");
$colonne = fgetcsv($handle, 1000, ",");
echo "<table>";
while(($righe = fgetcsv($handle, 1000, ",")) !== false){
    echo "<tr>";
    foreach ($righe as $dato) {
        echo "<td>" . htmlspecialchars($dato) . "</td>";
    }
    echo "</tr>";
}
echo "</table>";
fclose($handle);
?>
```

L'esempio sopra utilizza la funzione ```fgetcsv()``` per leggere il contenuto di un file CSV, indicando che il file è stato aperto con il codice ```fopen()```. Ciò assicura che il file sia chiuso correttamente dopo l'uso.

## Approfondimenti:

Il formato CSV è stato sviluppato negli anni '70 come metodo standard per l'importazione e l'esportazione di dati da fogli di calcolo e database. Nel tempo, sono stati sviluppati altri formati di file per la gestione dei dati tabulari, come il formato JSON. Tuttavia, CSV rimane ancora popolare e funziona bene con molti tipi di dati.

Alcuni sviluppatori preferiscono utilizzare le librerie di terze parti per lavorare con CSV in PHP, come ad esempio la popolare libreria "league/csv". Queste librerie offrono funzionalità aggiuntive e una maggiore flessibilità rispetto alle funzioni integrate di PHP.

Per implementare in modo efficiente un'elaborazione di grandi quantità di dati CSV, è consigliabile utilizzare un parser CSV, come ad esempio "fgetcsv()", che legge una sola riga di dati alla volta, invece di caricare l'intero file in memoria.

## Vedi anche:

- Documentazione PHP: [Funzione fgetcsv()](https://www.php.net/manual/en/function.fgetcsv.php)
- Libreria "league/csv": [https://csv.thephpleague.com/](https://csv.thephpleague.com/)