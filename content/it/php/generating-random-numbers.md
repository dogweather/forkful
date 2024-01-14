---
title:    "PHP: Generazione di numeri casuali"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

La generazione di numeri casuali è un'abilità fondamentale nella programmazione PHP. Può essere utilizzata per una varietà di scopi, come generare password sicure, simulare giochi, generare dati di test e altro ancora. Imparare come generare numeri casuali è un'abilità importante per qualsiasi programmatore PHP.

## Come

La generazione di numeri casuali in PHP è molto semplice e si può fare facilmente utilizzando la funzione `rand()` o la funzione `mt_rand()`.

```PHP
// Generare un numero casuale tra 1 e 100
$num = rand(1, 100);
echo $num;

// Generare un numero casuale tra 0 e 10
$altro_num = mt_rand(0, 10);
echo $altro_num;
```

Questo esempio utilizza la funzione `rand()` per generare un numero casuale tra 1 e 100 e la funzione `mt_rand()` per generare un numero casuale tra 0 e 10. Entrambe le funzioni accettano due argomenti: il primo è il numero minimo e il secondo è il numero massimo del range in cui generare il numero casuale.

## Deep Dive

La funzione `rand()` utilizza un generatore di numeri pseudo-casuali che produce numeri casuali utilizzando un algoritmo matematico. Tuttavia, questo metodo non garantisce che i numeri generati siano veramente casuali, ma solo pseudo-casuali. Per ottenere numeri veramente casuali, è possibile utilizzare la funzione `mt_rand()` che utilizza un generatore di numeri basato su un algoritmo Mersenne Twister.

Inoltre, è possibile specificare un seed per entrambe le funzioni, che è un valore iniziale utilizzato dal generatore di numeri per iniziare la sequenza di numeri casuali. Ciò può essere utile per riprodurre una sequenza di numeri specifica.

## Vedi anche

- [Funzione rand()](https://www.php.net/manual/en/function.rand.php)
- [Funzione mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [Generatore di numeri pseudo-casuali](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Algoritmo Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)