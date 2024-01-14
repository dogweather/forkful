---
title:    "PHP: Scrivere un file di testo"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione fondamentale nella programmazione PHP. Con l'utilizzo di poche righe di codice, è possibile creare e salvare un file di testo contenente informazioni importanti per il nostro programma. In questo articolo, esploreremo il processo di scrittura di un file di testo utilizzando PHP.

## Come fare

Per iniziare, è necessario aprire un nuovo file PHP e impostare il tag di apertura ```<?php``` all'inizio del documento. Successivamente, inseriremo la seguente riga di codice per creare un nuovo file di testo:

```PHP
$myfile = fopen("testfile.txt", "w") or die("Impossibile creare il file!");
```

In questa riga di codice, stiamo utilizzando la funzione ```fopen()``` per creare un nuovo file di testo chiamato "testfile.txt" nella directory in cui si trova il nostro file PHP. Il secondo parametro "w" indica che vogliamo aprire il file in modalità scrittura.

Per scrivere del testo nel nostro file di testo, utilizzeremo la funzione ```fwrite()``` inserendo il testo da scrivere e il puntatore del file creato in precedenza come parametri:

```PHP
fwrite($myfile, "Hello world!");
```

Infine, dobbiamo ricordare di chiudere il file dopo averlo scritto utilizzando la funzione ```fclose()```:

```PHP
fclose($myfile);
```

Ecco un esempio completo di come scrivere un file di testo utilizzando PHP:

```PHP
<?php
$myfile = fopen("testfile.txt", "w") or die("Impossibile creare il file!");
fwrite($myfile, "Hello world!");
fclose($myfile);
?>
```

## Approfondimenti

Oltre alla scrittura dei file di testo, PHP offre anche la possibilità di leggere e modificare i file esistenti. Utilizzando la funzione ```fread()```, è possibile leggere il contenuto di un file e utilizzando la funzione ```file_put_contents()```, è possibile aggiungere o modificare il contenuto di un file esistente. È importante ricordare di gestire correttamente gli errori durante le operazioni di lettura e scrittura dei file con le funzioni ```fopen()``` e ```fclose()```.

## Vedi anche

- [PHP File Handling](https://www.php.net/manual/en/book.filesystem.php)
- [PHP File Functions](https://www.w3schools.com/php/php_ref_filesystem.asp)
- [PHP Error Handling](https://www.php.net/manual/en/book.errorfunc.php)