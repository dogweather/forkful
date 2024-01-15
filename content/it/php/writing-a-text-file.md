---
title:                "Scrivere un file di testo"
html_title:           "PHP: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione molto comune nella programmazione PHP. Può essere utile per salvare dati o informazioni importanti che devono essere conservati anche dopo che il programma è terminato.

## Come fare

Per scrivere un file di testo con PHP, segui questi semplici passaggi:

1. Utilizza la funzione `fopen()` per aprire il file e stabilire il modo in cui il file verrà utilizzato (lettura, scrittura, ecc.).
2. Utilizza la funzione `fwrite()` per scrivere il contenuto del file.
3. Utilizza la funzione `fclose()` per chiudere il file e salvare le modifiche.

Esempio di codice:

```PHP
$file = fopen("test.txt", "w+"); // apre il file in modalità scrittura e creazione
fwrite($file, "Questo è un esempio di testo.");
fclose($file);
```

Output:

Il file "test.txt" verrà creato nella stessa directory in cui si trova il file PHP e conterrà il testo "Questo è un esempio di testo.".

## Approfondimenti

Esistono diverse opzioni che possono essere utilizzate con le funzioni `fopen()`, `fwrite()` e `fclose()`. Alcune di queste opzioni includono la lettura e scrittura di file binari, la modifica dei permessi dei file e la scrittura di file CSV.

La funzione `fopen()` accetta diversi parametri facoltativi, come ad esempio il tipo di codifica del file, il tipo di buffer utilizzato e il tipo di blocco dei file. È importante scegliere i parametri giusti in base alle proprie esigenze.

## Vedi anche

- Documentazione ufficiale PHP per le funzioni `fopen()`, `fwrite()` e `fclose()`: https://www.php.net/manual/en/ref.filesystem.php
- Esempi di codice per scrivere file di testo: https://www.w3schools.com/php/php_file_write.asp
- Utilizzo avanzato delle funzioni `fopen()`, `fwrite()` e `fclose()`: https://www.geeksforgeeks.org/php-fopen-fwrite-and-fclose/