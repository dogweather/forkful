---
title:                "Lavorare con i file CSV"
html_title:           "PHP: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati tabellari, come ad esempio elenchi di prodotti o dati di contabilità, è probabile che ti troverai a dover manipolare i dati in formato CSV (Comma Separated Values). PHP offre una semplice e potente libreria per gestire i file CSV, che ti permette di leggere, scrivere e manipolare facilmente i dati.

## Come fare

Per iniziare a lavorare con file CSV in PHP, prima di tutto assicurati di avere la versione più recente del linguaggio installata sul tuo computer. Successivamente, segui questi passaggi:

1. Importa la libreria CSV di PHP utilizzando il comando `use`:
 
`use \SplFileObject;`

Questo ti permetterà di utilizzare le funzionalità di lettura e scrittura fornite dalla libreria.

2. Imposta una variabile con il nome del tuo file CSV:

`$csvFile = "lista_prodotti.csv";`

3. Crea un'istanza della classe `SplFileObject`, specificando il nome del file come parametro:

`$file = new SplFileObject($csvFile);`

4. Per leggere i dati dal file CSV, puoi utilizzare un ciclo `foreach` che itera sulle righe del file:

```PHP
foreach($file as $line) {
  $data = str_getcsv($line); // Converte la riga in un array di dati
  // Fai qualcosa con i dati, ad esempio stampali a schermo:
  echo "Nome prodotto: " . $data[0] . PHP_EOL;
  echo "Prezzo: €" . $data[1] . PHP_EOL;
}
```

5. Per scrivere su un file CSV, puoi utilizzare il metodo `fputcsv` della classe `SplFileObject`:

```PHP
$nuovaRiga = ["Sedia da ufficio", "150.00"];
$file->fputcsv($nuovaRiga);
```

Questo scriverà una nuova riga con il nome del prodotto e il prezzo specificato nel tuo file CSV.

## Approfondimento

Oltre alle funzionalità di base di lettura e scrittura, la libreria CSV di PHP offre anche altri strumenti utili per lavorare con i dati. Ad esempio, puoi specificare un delimitatore diverso dal carattere virgola (`,`) utilizzando il metodo `setCsvControl`. Puoi anche specificare una riga di intestazione nel tuo file CSV e accedere ai dati utilizzando i nomi delle colonne invece degli indici numerici.

Per ulteriori informazioni e dettagli su tutte le funzioni disponibili, puoi consultare la documentazione ufficiale di PHP sulla libreria CSV.

## Vedi anche

- [Documentazione ufficiale di PHP sulla libreria CSV](https://www.php.net/manual/en/ref.csv.php)
- [Tutorial su come lavorare con file CSV in PHP](https://www.tutorialspoint.com/php/php_and_csv.htm)
- [Esempi di utilizzo della libreria CSV di PHP](https://www.techiediaries.com/php-csv-files-manipulation/)