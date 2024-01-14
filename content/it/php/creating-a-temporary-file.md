---
title:                "PHP: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché creare un file temporaneo?

Creare un file temporaneo è utile quando si desidera salvare temporaneamente dei dati o effettuare operazioni che richiedono l'utilizzo di un file. Ad esempio, potrebbe essere utile per gestire il caricamento di file su un sito web o per mantenere temporaneamente informazioni di sessione.

## Come creare un file temporaneo in PHP

Per creare un file temporaneo in PHP, è necessario utilizzare la funzione `tempnam()`. Questa funzione restituisce il percorso del nuovo file temporaneo creato nel sistema operativo. Di seguito è riportato un esempio di codice che crea un file temporaneo e ci scrive del testo all'interno.

```PHP
$temp_file = tempnam('/tmp', 'prefix_');
$file_handle = fopen($temp_file, 'w');
fwrite($file_handle, 'Questo è un file temporaneo');
fclose($file_handle);
```

Una volta eseguito, l'output dovrebbe essere un nuovo file temporaneo creato con il nome "prefix_randomcharacters". Il file dovrebbe contenere il testo "Questo è un file temporaneo".

## Approfondimento sulla creazione di file temporanei

La funzione `tempnam()` accetta due parametri: il primo è il percorso di base in cui il file temporaneo verrà creato e il secondo è un prefisso opzionale da aggiungere al nome del file. Questo prefisso è utile quando si desidera distinguere facilmente il file temporaneo da altri file nel percorso specificato.

Inoltre, è importante notare che la funzione `tempnam()` non crea effettivamente il file sul disco, ma restituisce semplicemente percorso e nome del file. Per creare effettivamente il file, è necessario utilizzare la funzione `fopen()` e scrivere del contenuto all'interno con `fwrite()`.

## Vedi anche

- [Documentazione ufficiale PHP sulla funzione tempnam()](https://www.php.net/manual/it/function.tempnam.php)
- [Come gestire file temporanei in PHP](https://www.html.it/pag/31851/gestire-file-temporan/)

Ti consigliamo inoltre di utilizzare la funzione `unlink()` per eliminare il file temporaneo una volta terminato l'utilizzo. In questo modo si evita di riempire il disco con file non necessari. Speriamo che questo articolo ti sia stato utile per imparare come creare e utilizzare file temporanei in PHP!