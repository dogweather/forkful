---
title:                "PHP: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su output di errore è un'abilità fondamentale per ogni programmatore PHP. Non solo aiuta a risolvere rapidamente eventuali problemi durante lo sviluppo, ma può anche fornire informazioni preziose per il debug dei programmi in produzione.

## Come Fare

Per scrivere su standard error in PHP, basta utilizzare la funzione `fwrite()`, specificando come primo parametro la costante `STDERR` e come secondo parametro il messaggio da scrivere. Esempio:

```PHP
fwrite(STDERR, "Errore: Impossibile trovare il file necessario.");
```

Questo scriverà il messaggio sull'output di errore, invece che sull'output standard.

## Approfondimento

Se il tuo programma riceve input utente, è importante essere sicuri che ogni messaggio di errore sia gestito correttamente. In caso contrario, potresti rischiare di divulgare informazioni sensibili ai tuoi utenti.

Inoltre, se stai sviluppando un'applicazione in cui siano coinvolti più processi, è fondamentale scrivere su standard error per monitorare i possibili problemi che potrebbero verificarsi.

## Vedi Anche

* [Scrittura su standard error in PHP](https://www.php.net/manual/en/function.fwrite.php)
* [Gestione degli errori in PHP](https://www.php.net/manual/en/book.errorfunc.php)
* [Debugging di applicazioni PHP in produzione](https://www.php.net/manual/en/debugger.prodserver.php)