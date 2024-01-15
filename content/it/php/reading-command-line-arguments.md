---
title:                "Lettura degli argomenti dalla riga di comando"
html_title:           "PHP: Lettura degli argomenti dalla riga di comando"
simple_title:         "Lettura degli argomenti dalla riga di comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori scelgono di utilizzare la riga di comando per eseguire i loro script PHP. La lettura degli argomenti dalla riga di comando può sembrare un dettaglio insignificante, ma in realtà è un'abilità importante che può semplificare e migliorare i tuoi script.

## Come fare

Per leggere gli argomenti dalla riga di comando in PHP, è possibile utilizzare la funzione `getopt()` . Questa funzione restituisce un array di opzioni e argomenti passati alla riga di comando.

```PHP
// Leggi gli argomenti dalla riga di comando
$options = getopt("a:b:c:");

// Stampa l'array di argomenti
print_r($options);

// Esempio di invio dalla riga di comando: php script.php -a Hello -b World
```

L'output di questo esempio sarà `Array ( [a] => Hello [b] => World )`. Nota che la lettera `a` e `b` corrispondono ai flag utilizzati nella funzione `getopt()`, mentre `Hello` e `World` sono gli argomenti passati dalla riga di comando.

Puoi anche utilizzare `getopt()` per accettare opzioni con parametri. Ad esempio, passando `-c test` dalla riga di comando, l'output sarà `Array ( [c] => test )`.

## Deep Dive

La funzione `getopt()` può accettare più flag e opzioni in una sola chiamata. Inoltre, è in grado di gestire flag abbreviati e opzioni non semplici (come numeri o stringhe). Questa funzione è particolarmente utile quando si lavora con script complessi che richiedono l'input dell'utente.

Inoltre, è importante notare che l'ordine degli argomenti è importante: gli argomenti devono essere passati alla riga di comando prima delle opzioni. Ad esempio, se il flag `a` richiede un parametro e il flag `b` non lo richiede, l'input `php script.php -a Hello -b World` funzionerà, mentre `php script.php -b World -a Hello` genererà un errore.

## Vedi anche

- [Documentazione ufficiale di PHP per la funzione getopt()](https://www.php.net/manual/en/function.getopt.php) 
- [Articolo su come utilizzare la funzione getopt() in PHP](https://www.javatpoint.com/how-to-use-getopt-function-in-php)