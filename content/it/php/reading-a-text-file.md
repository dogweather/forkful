---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?
Leggere un file di testo in programmazione significa accedere ai dati contenuti in un file di testo. Lo facciamo per recuperare, usare, manipolare o analizzare tali dati secondo i nostri requisiti.

## Come si fa:
```PHP
<?php
$file = 'miofile.txt';
$fopen = fopen($file, 'r');
if ($fopen) {
    while (($line = fgets($fopen)) !== false) {
        echo $line;
    }
    fclose($fopen);
} else {
    echo "Errore nell'apertura del file";
}
?>
```
Nell'esempio sopra, hai aperto `miofile.txt` e letto ogni riga una alla volta fino a raggiungere la fine del file. Se il file non può essere aperto, apparirà un messaggio di errore.

Uscita Esempio:
```TXT
Ciao, sono il contenuto di miofile.txt.
```

## Approfondimenti
(1) La lettura dei file di testo è un'operazione fondamentale eseguita in PHP fin dalla sua creazione. Le funzioni come `fgets()` e `fopen()` sono state introdotte nelle prime versioni di PHP e sono ancora ampiamente utilizzate.

(2) Esistono alternative come `file_get_contents()` per leggere tutto il file di testo in una volta. Tuttavia, per file di grandi dimensioni, è consigliato il metodo `fopen()` per evitare problemi di memoria.

(3) Il flag `'r'` nell'apertura del file sta per 'lettura'. PHP supporta diversi flag come `'w'` per la scrittura, `'a'` per l'aggiunta, `'x'` per la creazione, ecc. 

## Per Saperne Di Più

2. Stack Overflow - [How to read a large file line by line?](https://stackoverflow.com/questions/4204561/how-to-read-a-large-file-line-by-line)