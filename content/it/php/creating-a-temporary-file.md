---
title:    "PHP: Creare un file temporaneo"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può sembrare un'operazione superflua, ma in realtà è di fondamentale importanza per ottimizzare il lavoro dei programmatori PHP. I file temporanei sono utilizzati per archiviare dati temporanei o dati di backup, senza dover creare file permanenti. In questo modo, si può evitare un carico eccessivo di spazio di archiviazione e lavorare in modo più veloce ed efficiente.

## Come fare

Per creare un file temporaneo in PHP, è possibile utilizzare la funzione `tmpfile()`. Questa funzione restituisce un puntatore a un file temporaneo aperto in modalità di lettura e scrittura. Ecco un esempio di codice:

```PHP
// Apriamo il file temporaneo
$file = tmpfile();

// Scriviamo qualcosa all'interno del file
fwrite($file, "Questo è un file temporaneo!");

// Leggiamo il contenuto del file
echo fread($file, 1024);

// Chiudiamo e cancelliamo il file
fclose($file);
```

L'output di questo codice sarà "Questo è un file temporaneo!". Come si può notare, la creazione e la gestione di un file temporaneo in PHP è molto semplice e non richiede molti sforzi.

## Approfondimento

Oltre alla funzione `tmpfile()`, esistono anche altre opzioni per creare un file temporaneo in PHP, come ad esempio le funzioni `tempnam()` e `sys_get_temp_dir()`. Inoltre, per gestire i file temporanei in modo più efficiente, è consigliato utilizzare la classe `SplTempFileObject` che fornisce metodi aggiuntivi per la gestione di file temporanei.

È importante tenere presente che i file temporanei vengono eliminati automaticamente quando lo script PHP termina o quando il file viene chiuso. Inoltre, è possibile specificare un percorso di salvataggio per i file temporanei, se si vuole che essi non vengano cancellati alla fine dello script.

## Vedi anche

- [Documentazione PHP per la funzione `tmpfile()`](https://www.php.net/manual/en/function.tmpfile.php)
- [Documentazione PHP per la classe `SplTempFileObject`](https://www.php.net/manual/en/class.spltempfileobject.php)
- [Guida su come gestire i file temporanei in PHP](https://www.phpzag.com/how-to-handle-temporary-file-in-php/)