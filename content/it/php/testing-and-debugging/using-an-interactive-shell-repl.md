---
aliases:
- /it/php/using-an-interactive-shell-repl/
date: 2024-01-26 04:16:31.301948-07:00
description: "Una shell interattiva, o REPL (Read-Eval-Print Loop), ti permette di\
  \ scrivere ed eseguire codice PHP al volo. \xC8 ideale per la sperimentazione, il\
  \ debug o\u2026"
lastmod: 2024-02-18 23:08:55.972299
model: gpt-4-0125-preview
summary: "Una shell interattiva, o REPL (Read-Eval-Print Loop), ti permette di scrivere\
  \ ed eseguire codice PHP al volo. \xC8 ideale per la sperimentazione, il debug o\u2026"
title: Utilizzo di un interprete interattivo (REPL)
---

{{< edit_this_page >}}

## Cos'è e perché?
Una shell interattiva, o REPL (Read-Eval-Print Loop), ti permette di scrivere ed eseguire codice PHP al volo. È ideale per la sperimentazione, il debug o l'apprendimento, dato che puoi testare frammenti di codice senza l'onere di creare uno script completo.

## Come fare:
Avvia la REPL di PHP eseguendo `php -a` nel tuo terminale. Ecco un assaggio di come funziona:

```php
php > echo "Ciao, Mondo!";
Ciao, Mondo!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Puoi anche definire funzioni:

```php
php > function somma($a, $b) { return $a + $b; }
php > echo somma(5, 10);
15
```

## Approfondimento
Le REPL esistono in qualche forma sin dai primi giorni di LISP negli anni '60. La shell interattiva di PHP è meno avanzata rispetto a quelle di linguaggi come Python o JavaScript. Non persiste lo stato tra le sessioni e manca di funzionalità come l'auto-completamento. Per una REPL PHP più ricca di funzionalità, considera alternative come `psysh` o `boris`. Queste shell di terze parti offrono migliori strumenti di introspezione, completamento automatico delle tabulazioni e persino un debugger.

Sotto il cofano, la REPL di PHP funziona compilando ed eseguendo ogni riga di codice man mano che viene inserita. I limiti di questo approccio diventano evidenti con azioni come la ridefinizione delle classi, che non è possibile nella stessa sessione. È ottima per test semplici ma può diventare ingombrante per compiti complessi.

## Vedi anche
- [Manuale PHP - Shell interattiva](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: Una console per sviluppatori in tempo reale, debugger interattivo e REPL per PHP](https://psysh.org/)
- [Boris: Un piccolo REPL per PHP](https://github.com/borisrepl/boris)
