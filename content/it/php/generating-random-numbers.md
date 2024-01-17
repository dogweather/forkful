---
title:                "Generazione di numeri casuali"
html_title:           "PHP: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Generare numeri casuali è una pratica utilizzata dai programmatori per aggiungere casualità ai loro script e applicazioni. Questo permette di creare una maggiore varietà e imprevedibilità nei risultati, perfetti per giochi, simulazioni o qualsiasi contesto in cui sia necessaria una scelta casuale.

## Come:
Un modo semplice per generare numeri casuali in PHP è utilizzare la funzione `rand()`. Questa funzione prende due argomenti: il primo rappresenta il numero minimo possibile, mentre il secondo rappresenta il numero massimo, e restituisce un numero intero casuale compreso tra di essi.

```PHP
// Genera un numero casuale compreso tra 1 e 10
echo rand(1, 10); 
// Output potenziale: 7, 2, 9, 10, 3, ecc.
```

Ci sono anche alcune varianti di questa funzione, come ad esempio `mt_rand()` che utilizza un algoritmo migliore per garantire una maggiore casualità.

## Approfondimento:
La generazione di numeri casuali ha una lunga storia nella programmazione e in informatica in generale. Prima dell'arrivo del computer, si usavano metodi fisici come l'estrazione di palline da una borsa o il lancio di dadi. Inoltre, ci sono altri metodi per generare numeri casuali in PHP, come ad esempio utilizzando il timestamp dell'ora corrente o le funzioni `uniqid()` e `md5()`.

## Vedi anche:
- Documentazione ufficiale sulle funzioni di generazione di numeri casuali in PHP: http://php.net/manual/en/function.rand.php
- Un articolo su alcune alternative alla funzione `rand()`: https://www.pontikis.net/blog/best-php-random-functions