---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Generare numeri casuali è il processo di creazione di numeri in modo imprevedibile e senza alcun schema riconoscibile. I programmatori lo fanno per vari scopi: filtraggio di dati, creazione di situazioni uniche nei giochi, crittografia, simulazioni, e molto altro ancora.

## Come fare:

Elixir fornisce il modulo `:rand` per generare numeri casuali. Ecco come lo possiamo utilizzare:

```elixir
# Generare un numero casuale tra 0.0 (incluso) e 1.0 (escluso)
numero_casuale = :rand.uniform()
IO.puts numero_casuale
```

Il codice sopra produrrà un numero casuale tra 0.0 e 1.0. Se desideri generare un numero intero casuale entro un certo intervallo, usa `:rand.uniform/1`. Ecco come:

```elixir
# Generare un numero intero casuale tra 1 e 10
numero_casuale_intero = :rand.uniform(10)
IO.puts numero_casuale_intero
```

Questo produrrà un numero casuale tra 1 e 10.

## Approfondimento

Il modulo `:rand` Elixir usa un algoritmo chiamato [Mersenne Twister](https://it.wikipedia.org/wiki/Mersenne_twister). È noto per generare sequenze di numeri pseudo-casuali lunghe e con buone proprietà statistiche. Tuttavia, non è adatto per la crittografia dato che non è sufficientemente impredicibile.

In accordo con Erlang (la lingua madre di Elixir), il generatore di numeri casuali veniva inizializzato con lo stesso seme ("seed") ogni volta che veniva riavviato il sistema. Questo era un problema perché produceva le stesse sequenze di numeri casuali ad ogni riavvio. In Elixir recenti, il modulo `:rand` si inizializza con un seme casuale ad ogni avvio, risolvendo il problema.

Se hai bisogno di generare numeri casuali per la crittografia, considera l'uso di librerie specifiche come [crypto](https://hexdocs.pm/elixir/Crypto.html).

## Leggi Anche

Per capire meglio la generazione di numeri casuali in Elixir, guarda questi risorse:

- Documentazione Elixir su `:rand`: [https://hexdocs.pm/elixir/1.12/Kernel.html#id4878](https://hexdocs.pm/elixir/1.12/Kernel.html#id4878)
- Esempi di codice dell'Elixir School:  [https://elixirschool.com/en/lessons/basics/basics/](https://elixirschool.com/en/lessons/basics/basics/)