---
title:                "PHP: Leggere un file di testo"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle attività più comuni nella programmazione. Ciò può essere utile per accedere a dati strutturati, manipolare informazioni o leggere file di configurazione. Questo articolo ti guiderà su come leggere un file di testo utilizzando PHP e le sue funzioni.

## Come Fare

Un modo semplice per leggere un file di testo utilizzando PHP è utilizzare la funzione `file_get_contents()`. Questa funzione accetta il percorso del file come argomento e restituisce il contenuto del file come una stringa.

```
<?php
$file_contents = file_get_contents("testo.txt");
echo $file_contents;
?>
```

L'output di questo codice sarà il contenuto del file di testo stampato a schermo.

Per leggere un file di testo riga per riga, è possibile utilizzare la funzione `file()`. Questa funzione restituisce un array in cui ogni elemento rappresenta una riga del file.

```
<?php
$file_content_array = file("testo.txt");

foreach($file_content_array as $line) {
    echo "$line <br>";
}
?>
```

Questo codice itera attraverso il contenuto del file riga per riga e stampa ogni riga a schermo.

## Approfondimento

Esistono diverse funzioni in PHP per leggere file di testo con maggiore precisione e controllo. Alcune di queste sono `fopen()`, `fread()`, `fgets()` e `fgetcsv()`. Ogni funzione ha un funzionamento e una sintassi leggermente diversi, quindi è importante leggere la documentazione ufficiale di PHP per scegliere quella più adatta alle tue esigenze.

Inoltre, è importante ricordare di chiudere sempre il file dopo averlo aperto utilizzando la funzione `fclose()`. Ciò garantisce che le risorse del sistema vengano gestite correttamente e previene eventuali errori o perdita di dati.

## Vedi Anche

- [PHP.net - Manipolazione dei file di testo](https://www.php.net/manual/it/ref.filesystem.php)
- [Tizlog - Lettura e scrittura di file di testo in PHP](https://tizlog.com/lettura-e-scrittura-dei-file-di-testo-in-php/)
- [Filippo De Santis - Lettura e scrittura di file in PHP](https://filippods.net/blog/2012/12/20/php-lettura-e-scrittura-di-file/)