---
title:                "Stampa output di debug"
html_title:           "PHP: Stampa output di debug"
simple_title:         "Stampa output di debug"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il "debug output" consiste nell'emettere informazioni sul proprio codice durante l'esecuzione del programma, allo scopo di individuare e correggere eventuali errori o problemi che si verificano durante l'esecuzione. Questa pratica è fondamentale per lo sviluppo di software di alta qualità e per migliorare l'efficienza del codice.

## Come fare:

Per stampare il "debug output" in PHP, è possibile utilizzare la funzione `print_r()`, che stampa una rappresentazione leggibile di una variabile o di un array. Ad esempio:

```PHP
$nome = "Mario";
echo "Il mio nome è: " . $nome;
```

Questo codice produrrà l'output: `Il mio nome è: Mario`

Un altro modo per emettere informazioni di debug è l'uso della funzione `var_dump()`, che mostra più informazioni, come il tipo di dato e la lunghezza di un array. Ad esempio:

```PHP
$numeri = array(1, 2, 3);
var_dump($numeri);
```

Questo codice produrrà l'output:

```
array(3) {
  [0]=>
  int(1)
  [1]=>
  int(2)
  [2]=>
  int(3)
}
```

## Approfondimento:

La stampa del "debug output" ha origini già nei primi tempi della programmazione, ed è stata utilizzata in molte lingue di programmazione diverse da PHP. Oltre alle funzioni `print_r()` e `var_dump()`, esistono anche altre alternative per emettere informazioni di debug, come ad esempio l'uso dei logger e l'integrazione di strumenti specifici come Xdebug.

In PHP, è anche possibile abilitare la modalità "debug" per visualizzare gli errori e le notifiche di debug direttamente nella pagina web. Per abilitare questa modalità, è necessario impostare `display_errors = On` e `error_reporting = E_ALL` nel file php.ini.

## Vedi anche:

- [PHP print_r() Function](https://www.php.net/manual/en/function.print-r.php)
- [PHP var_dump() Function](https://www.php.net/manual/en/function.var-dump.php)
- [Xdebug: A powerful debugging and profiling tool for PHP](https://www.xdebug.org/)