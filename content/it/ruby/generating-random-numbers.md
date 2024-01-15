---
title:                "Generazione di numeri casuali"
html_title:           "Ruby: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un compito molto comune in programmazione, soprattutto per scopi di gioco o per la simulazione di situazioni casuali. Utilizzare una funzione di generazione di numeri casuali può aumentare la varietà e l'emozione di un programma.

## Come fare

Per generare numeri casuali in Ruby, è possibile utilizzare il metodo `.rand()` o la classe `Random`. Ecco un esempio di come utilizzare `.rand()` per generare un numero intero casuale compreso tra 1 e 10:

```Ruby
random_number = rand(1..10)
puts random_number  # Stampa un numero casuale tra 1 e 10
```

Se invece si desidera generare un numero razionale casuale compreso tra 0.0 e 1.0, si può utilizzare `.rand()` senza specificare un intervallo:

```Ruby
random_decimal = rand()
puts random_decimal  # Stampa un numero razionale casuale tra 0.0 e 1.0
```

Utilizzando la classe `Random`, è possibile generare numeri casuali più precisi e controllare la generazione tramite un seme (seed) specifico. Ecco un esempio:

```Ruby
random = Random.new(123)  # Crea un'istanza di Random con un seme specifico
random_number = random.rand(100)  # Genera un numero intero casuale compreso tra 0 e 100
puts random_number
```

Per ulteriori dettagli sui metodi disponibili per la generazione di numeri casuali in Ruby, si può consultare la documentazione ufficiale su [https://ruby-doc.org/core-3.0.0/Random.html](https://ruby-doc.org/core-3.0.0/Random.html).

## Deep Dive

In Ruby, la generazione di numeri casuali si basa sull'algoritmo Mersenne Twister, che è un algoritmo di generazione di numeri pseudo-casuali altamente efficiente e ampiamente utilizzato in diversi linguaggi di programmazione. La classe `Random` utilizza questo algoritmo e permette di controllare la generazione dei numeri tramite il seme specificato.

Una cosa importante da tenere a mente quando si generano numeri casuali è che essi sono ancora basati su un algoritmo e quindi non sono completamente casuali. Tuttavia, per scopi di gioco o simulazioni, l'utilizzo di numeri pseudo-casuali è spesso sufficiente.

## Vedi anche

- [Documentazione ufficiale di Ruby su Random](https://ruby-doc.org/core-3.0.0/Random.html)
- [Algoritmo Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)