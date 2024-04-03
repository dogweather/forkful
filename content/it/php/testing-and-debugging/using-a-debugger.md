---
date: 2024-01-26 03:50:37.599592-07:00
description: "Come fare: PHP include un debugger interattivo chiamato Xdebug. Ecco\
  \ come usarlo. Prima, assicurati di avere Xdebug installato e configurato nel tuo\
  \ file\u2026"
lastmod: '2024-03-13T22:44:43.521233-06:00'
model: gpt-4-0125-preview
summary: PHP include un debugger interattivo chiamato Xdebug.
title: Utilizzo di un debugger
weight: 35
---

## Come fare:
PHP include un debugger interattivo chiamato Xdebug. Ecco come usarlo.

Prima, assicurati di avere Xdebug installato e configurato nel tuo file `php.ini`:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Successivamente, scrivi uno script PHP semplice con un bug:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Ops! Qui dovrebbe esserci un più, non un meno
}

$result = add(1, 2);
echo "Il risultato è: $result"; // Il risultato dovrebbe essere 3, non -1
```

Utilizzando un IDE come PhpStorm, imposta un punto di interruzione cliccando accanto al numero di riga. Avvia il debugger e osserva come cambiano le variabili mentre procedi con l'esecuzione. Quando passi sopra la funzione `add`, noterai che `$result` diventa -1, il che è inaspettato.

## Approfondimento:
Storicamente, PHP veniva utilizzato principalmente per piccoli script, e il debugging era una questione di aggiungere istruzioni `var_dump()` e `print_r()` in tutto il codice. Nel tempo, con PHP che è diventato un attore chiave nello sviluppo web, sono stati presi in uso strumenti più sofisticati come Xdebug e Zend Debugger.

Alternative a Xdebug includono pcov e phpdbg. Questi offrono varie caratteristiche ma potrebbero non essere completi come Xdebug. phpdbg è un debugger specifico per PHP, leggero, che è distribuito con PHP dalla versione 5.6, e pcov è un driver per la copertura del codice.

Quando implementi un debugger, ricorda che non dovresti mai lasciare il debugger attivato nel tuo server di produzione, poiché può esporre vulnerabilità di sicurezza e rallentare le prestazioni.

## Vedi Anche:
- [Documentazione di Xdebug](https://xdebug.org/docs/)
- [Guida al Debugging di PhpStorm](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net su phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov su GitHub](https://github.com/krakjoe/pcov)
