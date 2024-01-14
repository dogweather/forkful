---
title:    "PHP: Lettura di un file di testo."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Lettura di file di testo è una delle operazioni fondamentali nel mondo della programmazione. Spesso, i programmatori hanno bisogno di accedere ai dati contenuti in un file di testo e adattarli per i propri scopi. In questo articolo, esploreremo il processo di lettura di un file di testo utilizzando il linguaggio di programmazione PHP. Continua a leggere se sei interessato ad imparare come farlo!

## Come fare

Per leggere un file di testo in PHP, è necessario seguire i seguenti passaggi:

1. Apri il file utilizzando la funzione `fopen()`. Questa funzione accetta due argomenti: il percorso del file e la modalità di apertura (`r` per leggere il file).
2. Leggi il contenuto del file utilizzando la funzione `fgets()`. Questa funzione legge una riga alla volta dal file e restituisce una stringa.
3. Chiudi il file utilizzando la funzione `fclose()` per liberare la memoria.

Ecco un esempio di codice che legge un file di testo e stampa il contenuto su schermo:

```
$fhandle = fopen("file.txt", "r");

// lettura del file linea per linea
while (($line = fgets($fhandle)) !== false) {
  echo $line; // stampa la riga
}

fclose($fhandle); // chiude il file
```

Se il file di testo è strutturato in colonne, possiamo utilizzare la funzione `fgetcsv()` per leggere i valori separatamente e salvarli in un array.

```
$fhandle = fopen("file.csv", "r");

// lettura del file colonna per colonna
while (($data = fgetcsv($fhandle)) !== false) {
  // stampa la colonna 1 e 2
  echo $data[0] . " " . $data[1] . "\n";
}

fclose($fhandle); // chiude il file
```

Ecco un esempio dell'output che otterremo nel secondo caso:

```
Nome Cognome
Mario Rossi
Paolo Bianchi
```

## Approfondimento

Oltre alla funzione `fgetcsv()`, che ci permette di gestire file di testo strutturati, ci sono altre funzioni utili per la lettura dei file in PHP. Ad esempio, la funzione `file()` che restituisce un array contenente ogni riga del file oppure la funzione `file_get_contents()` che restituisce il contenuto del file come una stringa.

Inoltre, è possibile specificare il numero massimo di caratteri da leggere con la funzione `fread()`, o utilizzare la funzione `feof()` per verificare se si è raggiunto la fine del file.

La lettura di file di testo è solo l'inizio, ci sono molti altri aspetti legati alla gestione dei file e dei dati. Continua a esplorare il mondo della programmazione per scoprirne di più!

## Vedi anche

- Tutorial PHP su come scrivere su un file di testo: https://www.php.net/manual/en/function.fwrite.php
- Come leggere e scrivere file CSV in PHP: https://www.php.net/manual/en/function.fgetcsv.php
- Documentazione ufficiale di PHP sulle funzioni per la gestione dei file: https://www.php.net/manual/en/book.filesystem.php