---
title:                "Gestione degli errori"
aliases:
- /it/php/handling-errors/
date:                  2024-01-26T00:55:27.441360-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e perché?
La gestione degli errori in PHP riguarda la gestione e la risposta a condizioni che interrompono il flusso normale di un programma, come file mancanti o input di dati errati. I programmatori gestiscono gli errori per prevenire crash e per offrire agli utenti un'esperienza più fluida.

## Come fare:
In PHP, puoi gestire gli errori utilizzando i blocchi `try-catch` e puoi personalizzare il processo con gestori di errori personalizzati ed eccezioni.

```php
// Esempio di base try-catch
try {
  // Fai qualcosa di rischioso
  $file = fopen("fileinesistente.txt", "r");
} catch (Exception $e) {
  // Gestisci l'errore
  echo "Errore: " . $e->getMessage();
}

// Impostare un gestore di errori personalizzato
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Utilizzo delle eccezioni
class MyException extends Exception {}

try {
  // Fai qualcosa e lancia un'eccezione personalizzata
  throw new MyException("Errore personalizzato!");
} catch (MyException $e) {
  // Gestisci l'eccezione personalizzata
  echo $e->getMessage();
}

// Output di esempio:
// Errore: fopen(fileinesistente.txt): failed to open stream: No such file or directory
// Errore personalizzato!
```

## Approfondimento
Ai tempi, gli errori PHP erano più che altro avvisi e notifiche che non interrompevano l'esecuzione degli script. Con la maturazione del linguaggio, è stata adottata una più robusta gestione degli errori orientata agli oggetti tramite la classe Exception introdotta in PHP 5. Successivamente, PHP 7 è stato rilasciato con classi Error che differenziano finalmente tra errori ed eccezioni.

Prima dei blocchi `try-catch`, PHP utilizzava `set_error_handler()` per affrontare gli errori. `try-catch` è più pulito, più moderno. Ma i gestori di errori personalizzati hanno ancora il loro spazio, specialmente per il codice legacy o quando è necessario intercettare ciò che normalmente non sarebbero errori eccezionali.

L'interfaccia `Throwable` in PHP 7+ significa che sia che si tratti di un Error che di un'Exception, puoi catturare entrambi. Questo è utile perché ora non ti perdi errori critici di runtime, che prima erano più difficili da tracciare.

Alternative ai meccanismi incorporati di PHP includono librerie e framework che dispongono dei propri sistemi di gestione degli errori, offrendo più funzionalità come la registrazione degli errori su file o la visualizzazione di pagine di errore user-friendly.

## Vedi anche
- Documentazione ufficiale di PHP sulle Eccezioni: https://www.php.net/manual/it/language.exceptions.php
- PHP The Right Way sulla segnalazione degli errori: https://phptherightway.com/#error_reporting
- Manuale PHP sulla Gestione degli Errori: https://www.php.net/manual/it/book.errorfunc.php
