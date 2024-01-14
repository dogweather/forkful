---
title:                "Elixir: Generazione di numeri casuali"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Per molti programmatori, la generazione di numeri casuali è una parte fondamentale della programmazione. Ci sono molte situazioni in cui potresti aver bisogno di numeri casuali, come ad esempio nella creazione di giochi, nella crittografia o nel testing di algoritmi. Elixir offre un modo semplice e intuitivo per generare numeri casuali, il che lo rende una scelta popolare per molti sviluppatori.

## Come fare

Per generare numeri casuali in Elixir, puoi utilizzare la funzione `:rand.uniform/1`. Questa funzione prende un intero come argomento e restituisce un numero casuale compreso tra 0 e il numero specificato (escluso).

`````elixir
num = :rand.uniform(10)
IO.puts "Numero casuale tra 0 e 10: #{num}"
`````

Questo codice stamperà a schermo un numero casuale tra 0 e 10 (escluso), ad esempio: `Numero casuale tra 0 e 10: 7`

Inoltre, puoi anche utilizzare la funzione `:rand.seed/1` per inizializzare il generatore di numeri casuali con un numero di seed specifico. Ciò garantisce che le tue sequenze di numeri casuali saranno le stesse ogni volta che esegui il programma.

`````elixir
:rand.seed(1234) # Inizializza il generatore di numeri casuali con il numero di seed 1234
num = :rand.uniform(10)
IO.puts "Numero casuale tra 0 e 10: #{num}"
`````

## Approfondimento

Per generare numeri casuali più complessi, puoi utilizzare la libreria Elixir `:rand`. Questa libreria offre diverse funzioni, come ad esempio `:rand.uniform/2`, che può essere utilizzata per generare numeri casuali tra un intervallo specificato.

`````elixir
num = :rand.uniform(5, 10) # Genera un numero casuale tra 5 e 10 (esclusi)
IO.puts "Numero casuale tra 5 e 10: #{num}"
`````

Puoi anche utilizzare la funzione `:rand.seed/2` per inizializzare il generatore di numeri casuali con un numero di seed specifico e una stringa di caratteri. Ciò ti consente di controllare ancora di più la sequenza dei numeri casuali generati.

`````elixir
:rand.seed(1234, "seed") # Inizializza il generatore di numeri casuali con il numero di seed 1234 e la stringa "seed"
num = :rand.uniform(5, 10)
IO.puts "Numero casuale tra 5 e 10: #{num}"
`````

## Vedi anche

- Documentazione ufficiale di Elixir sulla generazione di numeri casuali: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/0
- Tutorial su Elixir random numbers di Elixir School: https://elixirschool.com/lessons/basics/random-numbers/
- Guida introduttiva su Elixir random numbers di Prograils: https://prograils.com/posts/elixir-random-numbers