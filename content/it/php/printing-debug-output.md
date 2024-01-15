---
title:                "Stampa dell'output di debug"
html_title:           "PHP: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando su un progetto PHP, potresti trovarlo utile stampare l'output di debug per comprendere meglio il codice e trovare eventuali errori. La stampa dell'output di debug ti permette di visualizzare informazioni dettagliate sulle variabili e sugli errori che possono aiutarti a risolvere i problemi più velocemente.

## Come fare

Puoi utilizzare la funzione `print_r()` per stampare un array in modo leggibile o `var_dump()` per visualizzare una rappresentazione estesa di una variabile.

```
<?php
$array = array('Ciao', 'mondo');
print_r($array);
// Output: 
// Array (
//      [0] => Ciao
//      [1] => mondo
// )
```

Puoi anche utilizzare la sintassi `echo` per stampare una stringa o il suo contenuto dinamico.

```
<?php
$nome = 'Mario';
echo "Ciao $nome";
// Output: Ciao Mario
```

Se hai bisogno di visualizzare informazioni di debug in una precisa parte del codice, puoi utilizzare la funzione `error_log()` per scrivere il tuo messaggio in un file di log.

```
<?php
$error = 'Errore nel caricamento dei dati';
error_log($error);
// Verrà scritto nel file di log
```

## Approfondimento

Stampare l'output di debug può essere utile durante lo sviluppo del progetto, ma è importante assicurarsi di rimuoverlo prima di portare il progetto in produzione. Inoltre, puoi utilizzare la funzione `die()` per interrompere l'esecuzione del codice e visualizzare un messaggio di errore se si verificano problemi durante l'esecuzione del codice.

## Vedi anche

- [Documentazione ufficiale di PHP su debug del codice](https://www.php.net/manual/en/debugger.php)
- [Tutorial su come utilizzare le funzioni di debug di PHP](https://www.phpzag.com/php-debugging-made-easy-with-print_r-and-var_dump/)
- [Esempi di utilizzo di `error_log()` per la gestione degli errori in PHP](https://www.sitepoint.com/how-to-use-the-error-log-to-debug-php-errors/)