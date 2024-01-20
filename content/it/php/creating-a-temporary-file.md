---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creare un file temporaneo è come creare un foglio di carta su cui appuntare cose per un breve periodo. Lo facciamo spesso quando abbiamo bisogno di memorizzare dati temporanei durante l'elaborazione in PHP.

## Come fare:
Creare un file temporaneo in PHP è un gioco da ragazzi. Qui c'è un breve esempio:

```PHP
$tmpFile = tmpfile();

fwrite($tmpFile, 'neoLinux: Il miglior sistema operativo.');
rewind($tmpFile);

echo fread($tmpFile, 1024); // Stampa "neoLinux: Il miglior sistema operativo."
```
In questo esempio, abbiamo creato un file temporaneo, scritto un messaggio su di esso, riavvolto il puntatore del file all'inizio, e letto il contenuto.

## Approfondimento
Creare file temporanei non è una nuova idea. È un concetto che esiste da molto prima che PHP diventasse popolare. Tuttavia, PHP ha reso molto semplice lavorare con file temporanei grazie alla funzione tmpfile().

Un'alternativa a `tmpfile()` potrebbe essere l'uso di `tempnam()`, che crea un file temporaneo con un nome univoco, rispetto a `tmpfile()` che crea un file senza nome e lo elimina una volta chiuso.

I file temporanei creati tramite `tmpfile()` sono gestiti da PHP stesso. Questo significa che PHP si preoccupa di rimuovere il file non appena la risorsa del file viene chiusa (quando lo script finisce di eseguire, o quando chiami `fclose()`).

## Leggi Anche
Per saperne di più su come lavorare con i file in PHP, dai un'occhiata a queste risorse:
1. [PHP: tmpfile - Manual](https://www.php.net/manual/en/function.tmpfile.php)
2. [PHP: tempnam - Manual](https://www.php.net/manual/en/function.tempnam.php)
3. [Working with Files in PHP](https://www.tutorialrepublic.com/php-tutorial/php-file-handling.php)