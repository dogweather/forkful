---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:08.705130-07:00
description: "Scrivere su standard error (stderr) in PHP riguarda l'indirizzare messaggi\
  \ di errore o diagnostica separatamente dall'output standard (stdout),\u2026"
lastmod: '2024-02-25T18:49:41.395583-07:00'
model: gpt-4-0125-preview
summary: "Scrivere su standard error (stderr) in PHP riguarda l'indirizzare messaggi\
  \ di errore o diagnostica separatamente dall'output standard (stdout),\u2026"
title: Scrivere sull'errore standard
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere su standard error (stderr) in PHP riguarda l'indirizzare messaggi di errore o diagnostica separatamente dall'output standard (stdout), consentendo agli sviluppatori di gestire meglio i propri flussi di output per il debugging e il logging. I programmatori utilizzano questa tecnica per garantire che i messaggi di errore non interferiscano con l'output del programma, rendendo più facile monitorare e risolvere problemi nelle applicazioni.

## Come:

In PHP, scrivere su stderr può essere realizzato utilizzando la funzione `fwrite()` insieme alla costante predefinita `STDERR`, che rappresenta il flusso di output degli errori.

```php
<?php
// Scrivere un semplice messaggio su stderr.
fwrite(STDERR, "Questo è un messaggio di errore.\n");
```

Output campione quando lo script viene eseguito dalla riga di comando:
```
Questo è un messaggio di errore.
```

Per dimostrare un uso più pratico, considera uno scenario in cui stai analizzando l'input dell'utente e incontri dati inaspettati:
```php
<?php
$input = 'dati inaspettati';

// Simulazione di un errore nel processare l'input dell'utente.
if ($input === 'dati inaspettati') {
    fwrite(STDERR, "Errore: Input inaspettato ricevuto.\n");
    exit(1); // Uscita con un valore non-zero per indicare un errore.
}
```

Sebbene le capacità integrate di PHP per gestire stderr siano generalmente sufficienti, quando si hanno a che fare con applicazioni più complesse o si desidera integrare il logging di stderr con sistemi esterni, librerie di terze parti come Monolog possono essere un potente alleato. Monolog è una libreria di logging che può gestire stderr tra molti altri destinatari (file, socket, ecc.).

Utilizzo di Monolog per scrivere su stderr:

Prima, assicurati di avere Monolog installato tramite Composer:
```
composer require monolog/monolog
```

Quindi, puoi configurare Monolog per usare `StreamHandler` indirizzato a `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Creare un canale di log
$log = new Logger('nome');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Aggiungere un messaggio di log su stderr
$log->warning('Questo è un messaggio di avvertimento.');
```

Il codice sopra utilizza Monolog per inviare un messaggio di avvertimento a stderr, che è particolarmente utile per applicazioni che richiedono configurazioni di logging dettagliate o monitoraggio dei log esterni.
