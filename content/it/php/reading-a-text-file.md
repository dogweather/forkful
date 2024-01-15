---
title:                "Lettura di un file di testo"
html_title:           "PHP: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle operazioni più comuni nella programmazione, soprattutto in PHP. Un file di testo contiene informazioni importanti che possono essere utilizzate per vari scopi, come l'elaborazione dei dati o la configurazione di un sito web.

## Come Fare

Per leggere un file di testo in PHP, è necessario utilizzare la funzione `file_get_contents()`. Questa funzione accetta come argomento il percorso del file da leggere e restituisce tutto il contenuto del file come una stringa. Ad esempio:

```
<?php
$file_content = file_get_contents("test.txt");
echo $file_content;
```

Il codice qui sopra legge il contenuto del file `test.txt` e lo stampa a video. In alternativa, è possibile utilizzare la funzione `fopen()` per aprire un file e utilizzare un ciclo `while` per leggere il suo contenuto riga per riga.

```
<?php
$handle = fopen("test.txt", "r");
while(!feof($handle)){
  $line = fgets($handle);
  echo $line;
}
```

Nel codice sopra, la funzione `feof()` viene utilizzata per determinare quando si è raggiunta la fine del file, mentre `fgets()` viene utilizzata per leggere una singola riga del file. È possibile utilizzare anche la funzione `file()` che legge un file e restituisce il suo contenuto come un array, con ogni elemento dell'array corrispondente a una riga del file.

```
<?php
$file_array = file("test.txt");
foreach($file_array as $line){
  echo $line;
}
```

## Approfondimento

Esistono anche altre opzioni per leggere un file di testo in PHP, come utilizzare la classe `SplFileObject` o utilizzare le funzioni `file()` e `stream_get_contents()` insieme. Inoltre, è possibile specificare diversi parametri aggiuntivi per la funzione `file_get_contents()`, come un limite di caratteri da leggere o la posizione da cui iniziare a leggere.

Se vuoi saperne di più su come lavorare con i file di testo in PHP, puoi consultare la documentazione ufficiale su `file_get_contents()` e le funzioni correlate.

## Vedi Anche

- [Documentazione ufficiale di PHP su file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Tutorial su come leggere un file di testo in PHP](https://www.php.net/manual/en/function.file-get-contents.php)
- [5 modi diversi per leggere un file di testo in PHP](https://www.php.net/manual/en/function.file-get-contents.php)