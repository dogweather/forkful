---
title:                "PHP: Scrivere su errore standard"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Perché scrivere su standard error in PHP

Scrivere su standard error in PHP è un'attività importante per i programmatori in quanto permette di identificare e risolvere rapidamente eventuali errori nel codice. Questo significa che, in caso di problemi, è possibile individuare con precisione la causa e correggerla facilmente, migliorando così l'efficienza e la qualità del proprio codice.

## Come fare

Scrivere su standard error in PHP è molto semplice. Basta utilizzare la funzione "error_log" seguita dal messaggio da scrivere tra parentesi. Ad esempio:

```PHP
<?php
    error_log("Errore: variabile non definita");
?>
```

Questo codice scriverà il messaggio di errore nella console o nel file di log di PHP, permettendo così di individuare l'errore e correggerlo.

## Approfondimento

Scrivere su standard error in PHP offre molte più possibilità di quelle che appaiono a prima vista. Ad esempio, è possibile specificare il tipo di errore da scrivere, utilizzare variabili per personalizzare il messaggio o scrivere su un file specifico.

Per specificare il tipo di errore, basta aggiungere un secondo parametro alla funzione "error_log" come nell'esempio seguente:

```PHP
<?php
    error_log("Errore: divisione per zero", 0);
?>
```

In questo caso, il numero "0" indica che il messaggio è semplicemente un errore generico. Altri valori possibili sono "1" per un errore di avvertimento o "2" per un errore fatale.

Inoltre, è possibile utilizzare variabili all'interno del messaggio per personalizzarlo in base al contesto in cui si verifica l'errore. Ad esempio:

```PHP
<?php
    $numero = 4;
    
    error_log("Errore: impossibile dividere per $numero");
?>
```

In questo caso, il messaggio di errore sarà "Errore: impossibile dividere per 4", consentendo una maggiore precisione nell'identificazione dell'errore.

Infine, è possibile scrivere il messaggio di errore su un file specifico anziché sulla console o sul file di log predefinito di PHP. Per farlo, basta specificare il percorso del file come terzo parametro della funzione "error_log".

## Vedi anche

* [Documentazione di PHP su error_log](https://www.php.net/manual/en/function.error-log.php)
* [Come gestire gli errori in PHP](https://www.php.net/manual/en/book.errorfunc.php)
* [Esempi di codice per scrivere su standard error in PHP](https://www.bullseye.io/error-handling-php.php)