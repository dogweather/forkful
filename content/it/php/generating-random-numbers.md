---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:50:01.901740-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Generare numeri casuali in PHP è come lanciare un dado virtuale per ottenere un risultato imprevedibile. I programmatori lo fanno per tutto: da alimentare algoritmi di crittografia a creare dati di esempio per testare applicazioni.

## How to (Come fare:)
In PHP, puoi generare numeri casuali con `rand()` o `mt_rand()`. Ecco un esempio con `rand()`:

```PHP
<?php
// Genera un numero casuale tra 1 e 10
$numeroCasuale = rand(1, 10);
echo $numeroCasuale;
?>
```

E con `mt_rand()`, che è più veloce e produce migliori risultati casuali:

```PHP
<?php
// Genera un numero casuale tra 1 e 10
$numeroCasuale = mt_rand(1, 10);
echo $numeroCasuale;
?>
```

Per i numeri casuali crittograficamente sicuri, usa `random_int()`:

```PHP
<?php
// Genera un numero casuale crittograficamente sicuro tra 1 e 10
$numeroCasuale = random_int(1, 10);
echo $numeroCasuale;
?>
```

## Deep Dive (Approfondimento)
Una volta, `rand()` era lo standard per generare numeri casuali in PHP. Tuttavia, non era abbastanza buono per tutti gli usi, specialmente per la crittografia. Così è nato `mt_rand()`, basato sull'algoritmo Mersenne Twister, noto per essere più veloce e avere una migliore distribuzione di casualità. 

Ma anche `mt_rand()` non è adatto per la sicurezza. Ecco perché PHP 7 ha introdotto `random_int()`, che utilizza sorgenti di casualità migliori, rendendolo adatto per la crittografia.

Un'altra funzione, `random_bytes()`, genera stringhe di byte casuali, utile per i token sicuri.

Ci sono alternative a queste funzioni, come l'utilizzo di generatori esterni (api, servizi cloud, ecc.), ma per la maggior parte delle applicazioni PHP, le funzioni integrate sono più che sufficienti.

## See Also (Vedi Anche)
- Documentazione ufficiale di PHP sulle funzioni di generazione di numeri casuali: https://www.php.net/manual/en/book.math.php
- Introduzione all'algoritmo Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
- Informazioni sulla sicurezza e sulla generazione di numeri casuali: https://www.php.net/manual/en/function.random-int.php