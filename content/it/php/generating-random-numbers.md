---
title:                "PHP: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'abilità importante per ogni programmatore. I numeri casuali sono ampiamente utilizzati in molti contesti, come giochi, criptografia e test di progettazione.

## Come Fare

Per generare numeri casuali in PHP, è possibile utilizzare la funzione ```mt_rand()``` che genera numeri interi casuali o ```mt_rand(min,max)```per specificare un valore minimo e massimo. Ad esempio, per generare un numero casuale compreso tra 1 e 10, si può usare il seguente codice:

```PHP
rand(1,10);
```

Il risultato sarà un numero intero compreso tra 1 e 10, ad esempio 5.

## Approfondimento

Oltre alla funzione ```mt_rand()```, PHP ha altre funzioni per la generazione di numeri casuali come ```rand()``` e ```shuffle()```. Inoltre, è possibile utilizzare la funzione ```srand(seed)``` per impostare un valore di inizializzazione per la generazione di numeri casuali.

Una considerazione importante quando si lavora con numeri casuali è la loro reale casualità. PHP utilizza un algoritmo per generare i numeri casuali e se questo algoritmo è conosciuto, i numeri possono diventare prevedibili. Quindi, se si sta lavorando su progetti sensibili, è consigliabile utilizzare librerie esterne o fonti esterne per generare numeri casuali più sicuri.

## Vedi Anche

- [Documentazione PHP: Generazione di numeri casuali](https://www.php.net/manual/en/book.math.php)
- [PHP: Funzione di generazione numeri casuali funzione](https://www.php.net/manual/en/function.rand.php)
- [PHP: Funzione di mischia array](https://www.php.net/manual/en/function.shuffle.php)