---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Generare numeri casuali significa creare numeri senza un modello apparente o prevedibile. I programmatori lo fanno per una serie di motivi, come simulazioni, test e giochi.

## Come fare:

In PHP, ci sono diversi modi per generare numeri casuali. Ecco alcuni esempi.

1) Utilizzando la funzione rand():
```PHP
<?php
  echo rand();
?>
```
L'output sarà un numero casuale tra 0 e "getrandmax".

2) Per generare un numero casuale tra un intervallo specifico, possiamo passare due parametri a rand():
```PHP
<?php
  echo rand(10, 30);
?>
```
L'output sarà un numero casuale tra 10 e 30.

## Approfondimenti:
 
1) Contesto storico: PHP ha avuto la funzione rand() fin dalla sua prima versione. Con PHP 7.1, è stata introdotta la funzione random_int() per generare numeri casuali crittograficamente sicuri.

2) Alternative: Oltre a rand(), PHP offre funzioni come mt_rand() (che usa l'algoritmo Mersenne Twister per una generazione più veloce) e random_int() (che è la scelta migliore per la sicurezza crittografica).

3) Dettagli di implementazione: Mentre rand() e mt_rand() generano numeri pseudo-casuali, random_int() genera numeri casuali crittograficamente sicuri. Questo la rende più sicura per casi di utilizzo come la generazione di token casuali.

## Leggi anche:

- PHP Manual: Funzione rand() (https://www.php.net/manual/it/function.rand.php)
- PHP Manual: Funzione mt_rand() (https://www.php.net/manual/it/function.mt-rand.php)
- PHP Manual: Funzione random_int() (https://www.php.net/manual/it/function.random-int.php)
- Sicurezza dei numeri casuali in PHP: (https://paragonie.com/blog/2015/07/how-safely-generate-random-strings-and-integers-in-php)