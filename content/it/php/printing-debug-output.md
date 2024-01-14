---
title:                "PHP: Stampa dell'output di debugging"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'azione fondamentale nella programmazione PHP. Rappresenta uno strumento potente per comprendere cosa sta effettivamente accadendo nel codice e può aiutare a individuare errori e bug.

## Come fare

Per stampare l'output di debug, è sufficiente utilizzare la funzione `var_dump()`. Ad esempio, se si vuole visualizzare il contenuto di una variabile `$nome`, basta scrivere:

```PHP
var_dump($nome);
```

Questo mostrerà il valore della variabile e il suo tipo. Per stampare solo il valore, si può utilizzare `print_r()`. Inoltre, è possibile inserire più variabili come argomenti della funzione per visualizzarle tutte in una sola chiamata.

## Approfondimento

La stampa di output di debug è particolarmente utile per analizzare variabili complesse come array e oggetti. Per esempio, se si ha un array associativo con una struttura complessa, grazie alla stampa di output di debug sarà possibile visualizzare in modo dettagliato tutti i suoi elementi e tipi di dati.

Inoltre, è importante ricordare di disattivare la stampa di output di debug nei sistemi in produzione, in quanto può influire negativamente sulle prestazioni dell'applicazione.

## Vedi anche

- [Documentazione di PHP su var_dump()](https://www.php.net/manual/it/function.var-dump.php)
- [Tutorial su come utilizzare la stampa di output di debug in PHP](https://www.html.it/pag/51931/utilizzare-var_dump-e-print-r-per-fare-output-di-debug-in-php/)