---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

La stampa del debug output è un metodo che ci consente di visualizzare i dati negli script PHP. I programmatori lo fanno per individuare e risolvere facilmente gli errori nel codice.

## Come fare:

Ecco un semplice esempio di come utilizzare la funzione `print_r` per stampare il debug output:

```PHP
<?php
  $arr = array('Uno', 'Due', 'Tre');
  print_r($arr);
?>
```

Risultato:

```PHP
Array
(
    [0] => Uno
    [1] => Due
    [2] => Tre
)
```

Inoltre, PHP fornisce anche la funzione `var_dump` per stampare il debug output:

```PHP
<?php
  $arr = array('Uno', 'Due', 'Tre');
  var_dump($arr);
?>
```

Risultato:

```PHP
array(3) {
  [0]=>
  string(3) "Uno"
  [1]=>
  string(3) "Due"
  [2]=>
  string(3) "Tre"
}
```

## Approfondimenti:

La stampa del debug output esiste da quando sono nate le prime lingue di programmazione e ha progressivamente guadagnato rilevanza con lo sviluppo dei software. Prima dell'introduzione di Xdebug, la stampa del debug output era uno dei principali metodi utilizzati dai programmatori PHP.

Parallelamente a `print_r` e `var_dump`, esiste anche `var_export` che rappresenta un'alternativa. La principale differenza è in come questi metodi presentano l'output. Ad esempio, `var_export` restituisce un output leggibile dal PHP stesso, che può essere utilizzato per ricostruire la variabile.

A livello di implementazione, tenete a mente che `print_r` e `var_dump` presentano le informazioni in modi diversi. `print_r` è messo in forma leggibile, mostrando i valori dell'array. `var_dump`, d'altro canto, mostra più dettagli, come il tipo e la dimensione dei dati.

## Vedi Anche:

- Funzione [print_r](https://www.php.net/manual/en/function.print-r.php) nella Guida ufficiale del PHP
- Funzione [var_dump](https://www.php.net/manual/en/function.var-dump.php) nella Guida ufficiale del PHP
- Funzione [var_export](https://www.php.net/manual/en/function.var-export.php) nella Guida ufficiale del PHP
- [Xdebug](https://xdebug.org/docs/), un potente strumento di debug per PHP.