---
title:                "Leggere un file di testo"
html_title:           "PHP: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Lettura di un file di testo è un'operazione fondamentale nella programmazione PHP. Consiste nell'aprire un file di testo, leggerne il contenuto e memorizzarlo in una variabile. I programmatori spesso utilizzano questa operazione per estrapolare informazioni da un file di testo, come ad esempio dati da un database esterno o log di errori.

## Come fare:
Ecco un esempio di codice per leggere un file di testo in PHP:
```
file_get_contents("nome_file.txt");
```
Il codice sopra utilizza la funzione `file_get_contents()` per aprire e leggere il contenuto del file di testo specificato. Il contenuto sarà quindi restituito come una stringa, che può essere utilizzata per ulteriori elaborazioni.

## Approfondimento:
La lettura di un file di testo è stata una funzionalità introdotta in PHP dalla versione 4.3.0 e ha da allora subito diverse evoluzioni e migliorie. Esistono anche altre alternative per leggere un file di testo, come ad esempio l'utilizzo delle funzioni `fopen()` e `fread()`. È importante notare che la lettura di un file di testo può essere influenzata dal sistema operativo utilizzato, ad esempio alcuni sistemi Windows potrebbero richiedere l'utilizzo di `fopen()` con il parametro "rb" per una lettura corretta.

## Vedi anche:
Per ulteriori informazioni sulla lettura di file di testo in PHP, consulta questi link:
- [Documentazione ufficiale di PHP su file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Tutorial su come leggere e scrivere file di testo in PHP](https://www.tutorialrepublic.com/php-tutorial/php-file-handling.php)