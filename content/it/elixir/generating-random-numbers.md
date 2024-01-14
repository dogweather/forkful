---
title:                "Elixir: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Perché

Generare numeri casuali è una delle attività più comuni in programmazione, soprattutto quando si lavora con giochi, simulazioni o algoritmi di ottimizzazione. Imparare a generare numeri casuali con Elixir può aprire le porte a nuove possibilità di programmazione e divertimento!

## Come fare

Per generare numeri casuali in Elixir, utilizzeremo la funzione `:rand.uniform/1`. Questa funzione prende come parametro un numero intero e restituisce un numero casuale compreso tra 0 e il numero specificato.

Per iniziare, apriamo il nostro terminale Elixir e scriviamo:

```Elixir
:rand.uniform(10)
```

Questo dovrebbe restituire un numero casuale tra 0 e 10, ad esempio `4`.

Se volessimo invece un numero casuale compreso tra 1 e 10, possiamo semplicemente aggiungere 1 al risultato:

```Elixir
:rand.uniform(10) + 1
```

In questo modo il risultato finale potrebbe essere ad esempio `7`.

Se vogliamo ottenere un numero casuale con decimali, possiamo utilizzare la funzione `:rand.uniform/2`, che prende come parametri un valore minimo e un valore massimo. Ad esempio:

```Elixir
:rand.uniform(1.0, 10.0)
```

Questo restituirà un numero casuale con un massimo di una cifra decimale compreso tra 1.0 e 10.0, ad esempio `7.2`.

## Approfondimento

La funzione `:rand.uniform/1` utilizza un algoritmo di generazione di numeri pseudo-casuali. Ciò significa che i numeri prodotti non sono veramente casuali, ma vengono generati da una formula matematica che produce numeri che sembrano casuali. Tuttavia, per la maggior parte dei casi di utilizzo, questo è più che sufficiente.

Elixir offre anche la possibilità di generare numeri veramente casuali utilizzando la funzione `:rand.seed/1`. Questa funzione utilizza come parametro una chiave di seed per generare un numero casuale. Tuttavia, questo viene fortemente sconsigliato in quanto può essere più lento e meno efficiente dell'algoritmo di generazione pseudo-casuale.

## Vedi anche

- [Documentazione Elixir: rand](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/1)
- [Guida alla programmazione funzionale con Elixir](https://www.expressivecode.com/functional-programming-in-elixir/)
- [How to Generate Random Numbers with Elixir](https://www.youtube.com/watch?v=9YlIsPx7Q9k)