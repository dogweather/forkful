---
title:    "PHP: Scrivere su standard error"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere sulla standard error è una tecnica vitale per il debugging e la gestione degli errori in PHP. Quando un errore si verifica, invece di interrompere completamente l'esecuzione dello script, scrivere sulla standard error consente di visualizzare l'errore senza interrompere il flusso.

## Come fare

Per scrivere sulla standard error in PHP, basta utilizzare la funzione *error_log()*, specificando il messaggio di errore come primo argomento e il tipo di output come secondo argomento. Ad esempio:

```PHP
$error_message = "Errore: file non trovato";
error_log($error_message, 3);
```

Il secondo argomento in questo esempio, *3*, indica che il messaggio di errore verrà scritto sulla standard error.

## Approfondimento

Se si desidera gestire gli errori in modo più avanzato, si può utilizzare la funzione *set_error_handler()* per definire una funzione personalizzata che gestirà tutti gli errori. In questo modo, è possibile specificare il tipo di output per ogni tipo di errore e scrivere messaggi personalizzati. Ad esempio:

```PHP
function custom_error_handler($errno, $errstr, $errfile, $errline, $errcontext) { 
    switch ($errno) { 
        case E_USER_ERROR: 
            error_log("Errore critico: $errstr", 3); 
            break; 
        case E_USER_WARNING: 
            error_log("Avviso: $errstr", 3); 
            break; 
        case E_USER_NOTICE: 
            error_log("Notifica: $errstr", 3); 
            break; 
        default: 
            error_log("Errore sconosciuto: $errstr", 3); 
            break; 
    } 
} 

set_error_handler("custom_error_handler");

// Esempio di utilizzo 
$file = "missing_file.php"; 
if (!file_exists($file)) { 
    trigger_error("File non trovato", E_USER_ERROR); 
} 
```

In questo esempio, la funzione *custom_error_handler()* gestisce diversi tipi di errori e li scrive sulla standard error utilizzando la funzione *error_log()*.

## Vedi anche

- [PHP error_log function](https://www.php.net/manual/en/function.error-log.php)
- [PHP set_error_handler function](https://www.php.net/manual/en/function.set-error-handler.php)
- [PHP error types](https://www.php.net/manual/en/errorfunc.constants.php) (tipi di errore PHP)