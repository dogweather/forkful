---
title:                "Stampa dell'output di debug"
aliases: - /it/php/printing-debug-output.md
date:                  2024-01-20T17:52:52.687790-07:00
model:                 gpt-4-1106-preview
simple_title:         "Stampa dell'output di debug"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Stampare l'output di debug aiuta a vedere cosa sta succedendo nel codice. Lo facciamo per trovare gli errori più velocemente e capire meglio il flusso del programma.

## How to: (Come fare:)
```PHP
<?php
// Semplice echo per il debugging
$variabile = "Ciao mondo!";
echo $variabile; // Stampa: Ciao mondo!

// Stampa strutturata con print_r
$lista = ['mele', 'banane', 'arance'];
echo '<pre>'; print_r($lista); echo '</pre>'; // Mostra l'array in modo leggibile

// Debugging con var_dump
$numero = 42;
var_dump($numero); // Stampa: int(42)
```

## Deep Dive (Approfondimento)
La stampa per il debug in PHP risale ai primi giorni del linguaggio. Con il tempo, sono emerse funzioni come `print_r()`, `var_dump()` e `var_export()` per output più dettagliati. Mentre `echo` e `print` sono buoni per output semplici, `var_dump()` mostra anche i tipi e le lunghezze delle variabili. In alternativa, per un controllo ancora più fine, possiamo usare xdebug, un'estensione di PHP che aggiunge molte funzionalità di debug.

## See Also (Vedi Anche)
- [PHP Manual: Echo](https://www.php.net/manual/en/function.echo.php)
- [PHP Manual: var_dump](https://www.php.net/manual/en/function.var-dump.php)
- [PHP Manual: print_r](https://www.php.net/manual/en/function.print-r.php)
- [Xdebug — Debugger and Profiler Tool for PHP](https://xdebug.org/)
