---
date: 2024-01-26 01:06:31.626052-07:00
description: "Il logging \xE8 sostanzialmente paragonabile al mantenere un diario\
  \ per il proprio codice; \xE8 l'atto di registrare eventi, errori e altri punti\
  \ dati\u2026"
lastmod: '2024-03-11T00:14:17.121238-06:00'
model: gpt-4-1106-preview
summary: "Il logging \xE8 sostanzialmente paragonabile al mantenere un diario per\
  \ il proprio codice; \xE8 l'atto di registrare eventi, errori e altri punti dati\u2026"
title: "Registrazione delle Attivit\xE0 (Logging)"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il logging è sostanzialmente paragonabile al mantenere un diario per il proprio codice; è l'atto di registrare eventi, errori e altri punti dati significativi che si verificano quando l'applicazione viene eseguita. I programmatori lo fanno per tenere traccia di ciò che accade sotto il cofano, per individuare e risolvere problemi, e per mantenere una traccia di controllo per analisi successive o per scopi di conformità.

## Come fare:

PHP offre una funzione integrata di registrazione degli errori che è facile da usare. Basta inserire `error_log()` nel tuo codice per inviare un messaggio ai log del server. Puoi anche personalizzarlo per scrivere su un file specifico.

```php
<?php
// Registrazione di un messaggio informativo semplice
error_log("Questa è una voce di log informativa.");

// Registrazione di un messaggio di errore
error_log("Questa è una voce di log di errore.", 0);

// Registrazione su un file specificato
file_put_contents('/percorso/del/tuo/custom.log', "Una voce di log personalizzata.\n", FILE_APPEND);

// Utilizzo di Monolog per il logging strutturato
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Creazione del logger
$logger = new Logger('nome');
// Ora aggiungere alcuni gestori
$logger->pushHandler(new StreamHandler('/percorso/del/tuo/monolog.log', Logger::WARNING));

// Ora puoi usare il tuo logger
$logger->warning('Questa è una voce di log di avviso!');
$logger->error('Questa è una voce di log di errore!');
?>
```

Questo produrrà i tuoi log o al log del server o al tuo file specificato in formato testo semplice.

## Approfondimento:

Storicamente, gli sviluppatori PHP si sono affidati alla funzione `error_log()` o ai log di Apache/Nginx per rilevare problemi, ma ciò può essere caotico con la necessità di analizzare file di testo semplice e senza un modo facile per filtrarli o ordinarli. Inserire librerie di logging come Monolog, che hanno inaugurato l'era del logging strutturato in PHP. Queste soluzioni offrono un migliore controllo fornendo molteplici canali di registrazione, livelli di gravità e output formattato (come JSON, che è un sogno per l'analisi programmata).

Alternative a Monolog includono Log4php, KLogger e Log4php di Apache. Dal punto di vista dell'implementazione, una registrazione solida richiede non solo di scaricare dati a caso, ma di prendere in considerazione aspetti come la rotazione dei log, le strategie di archiviazione e l'integrazione con strumenti di monitoraggio per essere veramente utili.

Dovresti tenere a mente l'[Interfaccia Logger PSR-3](https://www.php-fig.org/psr/psr-3/), che delinea un'interfaccia comune per le librerie di logging, garantendo l'interoperabilità e un modo coerente per accedere ai meccanismi di logging.

## Vedi anche:

- [Repository GitHub di Monolog](https://github.com/Seldaek/monolog)
- [Specifica dell'Interfaccia Logger PSR-3](https://www.php-fig.org/psr/psr-3/)
- [Documentazione di PHP Error Log](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: Una Semplice Classe di Logging per PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Un versatile framework di logging per PHP](https://logging.apache.org/log4php/)

Fai pratica con le funzioni integrate, ma per un approccio più mantenibile e scalabile, considera di investire tempo per familiarizzare con una libreria come Monolog. Buona registrazione!
