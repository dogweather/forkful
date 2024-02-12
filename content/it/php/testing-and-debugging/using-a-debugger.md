---
title:                "Utilizzo di un debugger"
aliases: - /it/php/using-a-debugger.md
date:                  2024-01-26T03:50:37.599592-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Un debugger è uno strumento che aiuta i programmatori a capire cosa sta effettivamente facendo il loro codice mentre viene eseguito. È la lente di ingrandimento che ci permette di zoomare sui bug—quelle fastidiose problematiche che causano il crash dei nostri programmi o li inducono a fornire risposte errate—e schiacciarli. Usiamo i debugger perché ci risparmiano ore di istruzioni di stampa e giochi di indovinelli.

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
