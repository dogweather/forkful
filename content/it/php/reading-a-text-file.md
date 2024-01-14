---
title:                "PHP: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché leggere un file di testo in PHP?

Leggere un file di testo può sembrare una semplice operazione, ma in realtà è uno dei compiti fondamentali per un programmatore PHP. Con questa guida, imparerai come leggere i contenuti di un file di testo utilizzando il linguaggio di programmazione PHP.

## Come leggere un file di testo in PHP

Per leggere un file di testo in PHP, è necessario seguire questi passaggi:

1. Apri il file di testo utilizzando la funzione `fopen()` specificando il percorso del file e la modalità di apertura (ad esempio, "r" per lettura).

2. Leggi il contenuto del file utilizzando la funzione `fread()` specificando il puntatore al file e la dimensione dei dati da leggere.

3. Chiudi il file utilizzando la funzione `fclose()` per rilasciare eventuali risorse utilizzate durante la lettura.

Questo è un esempio di codice che mostra come leggere il contenuto di un file di testo utilizzando PHP:

```php
<?php
$file = fopen("test.txt", "r") or die("Impossibile aprire il file!");
// Leggi il contenuto del file
$content = fread($file, filesize("test.txt"));
fclose($file);
// Visualizza il contenuto del file
echo $content;
?>
```

L'output del codice sarà qualcosa del genere:

```
Questo è il contenuto del file di testo.
```

## Approfondimento sulla lettura di un file di testo

Oltre alla semplice lettura del contenuto di un file di testo, PHP offre diverse funzioni per manipolare e gestire i file. Ad esempio, è possibile utilizzare la funzione `fgets()` per leggere una singola riga del file, o la funzione `file()` per ottenere un array contenente tutte le righe del file.

Inoltre, è possibile specificare un offset e una lunghezza dei dati da leggere utilizzando la funzione `fseek()`, o scrivere dei dati in un file utilizzando la funzione `fwrite()`.

Per ulteriori informazioni su come leggere e gestire i file di testo in PHP, consulta la documentazione ufficiale su [php.net](http://php.net/manual/en/function.fread.php).

## Vedi anche

- [Come scrivere su un file di testo in PHP](https://www.php.net/manual/en/function.fopen.php)
- [Come manipolare i file in PHP](https://www.php.net/manual/en/ref.filesystem.php)