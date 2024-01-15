---
title:                "Conversione di una stringa in minuscolo"
html_title:           "PHP: Conversione di una stringa in minuscolo"
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante lo sviluppo di un progetto web, ci troveremo a gestire stringhe di testo che dovranno essere convertite in caratteri minuscoli. Questa operazione è importante per assicurarsi che le comparazioni tra stringhe siano corrette e per standardizzare la formattazione dei dati.

## Come fare

```PHP
$stringa = "Questa è una STRINGA DI TESTO";
$stringaMinuscola = strtolower($stringa);

echo $stringaMinuscola; // output: questa è una stringa di testo
```

Per convertire una stringa in caratteri minuscoli in PHP, possiamo utilizzare la funzione `strtolower()`. Questa funzione accetta come parametro la stringa da convertire in caratteri minuscoli e restituisce il risultato della conversione. Possiamo poi utilizzare il risultato come meglio desideriamo, ad esempio assegnarlo ad una variabile o stamparlo a schermo.

## Approfondimenti

La funzione `strtolower()` di PHP utilizza le regole del locale corrente per determinare come convertire i caratteri. Ciò significa che il risultato della conversione può variare in base alla configurazione del nostro server. Inoltre, se abbiamo bisogno di convertire una stringa in caratteri minuscoli in un locale specifico, possiamo utilizzare la funzione `setlocale()` prima di chiamare `strtolower()`.

## Vedi anche

- [PHP: strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [PHP: setlocale()](https://www.php.net/manual/en/function.setlocale.php)