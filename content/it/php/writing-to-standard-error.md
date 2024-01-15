---
title:                "Scrivere su standard error"
html_title:           "PHP: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

C'è un vecchio detto tra i programmatori che dice "Se qualcosa va storto, scrivilo su stderr". In altre parole, quando c'è un errore nel codice, è importante scrivere un messaggio di errore su stderr (standard error) in modo che il programmatore possa vedere cosa è andato storto e come risolverlo.

Scrivere su stderr è quindi una pratica essenziale per la risoluzione dei problemi e il debugging del codice.

## Come Fare

Per scrivere su stderr in PHP, possiamo utilizzare la funzione "fwrite ()" che ci consente di scrivere su un file specificato. In questo caso, il file specificato sarà il file di errore standard, che ha il file descriptor "2". Questo può essere fatto utilizzando la costante "STDERR" fornita dalla libreria PHP.

Consideriamo un semplice esempio per scrivere su stderr:

```PHP
$msg = "Errore nell'apertura del file.";
fwrite(STDERR, $msg);
```

Il codice sopra scriverà il messaggio di errore su stderr anziché sull'output standard. Vedremo il messaggio di errore solo se eseguiamo il codice da linea di comando o se il codice è eseguito su un server senza un gestore di output.

Oltre a utilizzare la funzione "fwrite ()", possiamo anche utilizzare la funzione "error_log ()" fornita da PHP per scrivere direttamente su stderr. Ad esempio:

```PHP
$msg = "Errore di connessione al database.";
error_log($msg, 0);
```

Questo esempio scriverà il messaggio di errore nel file di log specificato (di solito il file di errore standard) e anche su stderr.

## Approfondimento

Oltre al semplice utilizzo della funzione "fwrite ()" o "error_log ()", esistono altri modi per scrivere su stderr in PHP. Ad esempio, possiamo utilizzare il gestore di errori personalizzato di PHP impostato tramite la funzione "set_error_handler ()".

Quando si utilizza un gestore di errori personalizzato, gli errori verranno gestiti tramite una funzione a nostra scelta anziché essere visualizzati sullo schermo. Possiamo quindi scrivere il messaggio di errore su stderr utilizzando la funzione "fwrite ()" o "error_log ()".

Inoltre, è importante notare che utilizzare una libreria di log come Monolog o Log4PHP può semplificare ulteriormente il processo di scrittura su stderr, consentendoci di gestire i messaggi di errore in modo più efficiente e organizzato.

## Vedi Anche

- [La documentazione PHP per fwrite ()](https://www.php.net/manual/en/function.fwrite.php)
- [La documentazione PHP per error_log ()](https://www.php.net/manual/en/function.error-log.php)
- [La documentazione PHP per set_error_handler ()](https://www.php.net/manual/en/function.set-error-handler.php)
- [Monolog](https://github.com/Seldaek/monolog)
- [Log4PHP](https://logging.apache.org/log4php/)