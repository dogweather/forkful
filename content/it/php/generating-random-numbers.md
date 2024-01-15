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

## Perché

Ci sono molte ragioni per cui potresti voler generare numeri casuali in PHP. Ad esempio, potresti aver bisogno di generare password sicure per i tuoi utenti, creare dati di test per il tuo codice o simulare il lancio di un dado in un gioco online.

## Come fare

Per generare un numero casuale in PHP, puoi utilizzare la funzione `rand()`. Ad esempio, se vuoi generare un numero compreso tra 1 e 10, puoi utilizzare il seguente codice:

```PHP
$randomNumber = rand(1, 10);
echo $randomNumber; // Output: un numero casuale tra 1 e 10
```

Puoi anche generare un numero casuale con una precisione specifica utilizzando la funzione `mt_rand()`. Questa funzione è utile per generare numeri con una precisione decimale. Ad esempio, se vuoi generare un numero con una precisione di due decimali tra 1 e 5, puoi utilizzare il seguente codice:

```PHP
$randomNumber = mt_rand(100, 500) / 100;
echo $randomNumber; // Output: un numero casuale con due decimali tra 1 e 5
```

## Approfondimento

Esistono diversi algoritmi di generazione di numeri casuali in PHP, ognuno con le proprie caratteristiche e scopi. Per esempio, la funzione `rand()` utilizza un semplice algoritmo lineare congruente, mentre la funzione `mt_rand()` utilizza il più avanzato algoritmo Mersenne Twister.

Inoltre, è importante notare che i numeri generati dalla maggior parte di questi algoritmi non sono veramente casuali, ma sono basati su una "seme" iniziale. Se lo stesso seme viene utilizzato, verrà generata la stessa sequenza di numeri casuali. Per questo motivo, è importante utilizzare una seme diversa ad ogni utilizzo della funzione `rand()` o `mt_rand()`.

## Vedi anche

- [Documentazione PHP per la funzione rand()](https://www.php.net/manual/en/function.rand.php)
- [Documentazione PHP per la funzione mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [Generazione di numeri casuali in PHP](https://www.geeksforgeeks.org/php-program-to-generate-random-numbers/)