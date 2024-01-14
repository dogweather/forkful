---
title:    "Elixir: Generazione di numeri casuali"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Molte volte, quando si scrive codice, abbiamo bisogno di generare dei numeri casuali. Questo può essere utile per simulare situazioni o per implementare logiche complesse. In Elixir, abbiamo una funzione built-in per generare numeri casuali, che può semplificare molto il nostro lavoro.

## Come fare
Per generare un numero casuale in Elixir, possiamo utilizzare la funzione `Kernel.rand/1`. Questa funzione accetta come argomento il limite superiore del range di numeri che vogliamo generare. Ad esempio, possiamo generare un numero casuale tra 1 e 10 con il seguente codice:

```elixir
Kernel.rand(10)
```

Se volessimo generare un numero casuale tra 50 e 100, possiamo utilizzare `Kernel.rand/2`, che accetta due argomenti rappresentanti il limite inferiore e superiore del range. Quindi, possiamo scrivere:

```elixir
Kernel.rand(50, 100)
```

La funzione `Kernel.rand/1` restituirà sempre un numero intero, mentre `Kernel.rand/2` può restituire sia un intero che un float a seconda dei limiti specificati.

## Un approfondimento
Se vogliamo generare una sequenza di numeri casuali, possiamo utilizzare la funzione `Enum.random/2`. Questa funzione accetta come argomenti uno o più elementi di una lista e restituisce un elemento casuale della lista. Ad esempio, possiamo generare una lista di 5 numeri casuali tra 1 e 10 con il seguente codice:

```elixir
Enum.random(1..10, 5)
```

Possiamo anche generare un numero casuale utilizzando un seed. Un seed è un valore utilizzato dagli algoritmi di generazione di numeri casuali per iniziare a generare numeri. In Elixir, possiamo utilizzare il modulo `:rand`, che ci permette di specificare un seed per la generazione di numeri casuali. Ad esempio, se volessimo generare un numero casuale utilizzando un seed di 42, possiamo scrivere:

```elixir
:rand.seed(:rand.uniform, 42)
```

In questo modo, la successiva chiamata alla funzione `Kernel.rand/1` genererà sempre lo stesso valore casuale.

## Vedi anche
- [Documentazione di Elixir sulla generazione di numeri casuali](https://hexdocs.pm/elixir/Kernel.html#rand/1)
- [Spiegazione in italiano della generazione di numeri casuali in Elixir](https://stackabuse.com/random-number-generation-in-elixir/) 
- [Utilizzo di seeding e pattern matching per la generazione di numeri casuali in Elixir](https://medium.com/@isk2017/random-number-generation-pattern-matching-in-elixir-46eb969a3adf)