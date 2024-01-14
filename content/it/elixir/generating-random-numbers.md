---
title:    "Elixir: Generazione di numeri casuali"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'abilità fondamentale nel mondo della programmazione. Può essere utile per creare dati di test o per creare un'esperienza più dinamica all'interno dei nostri programmi.

## Come Fare

Per generare un numero casuale in Elixir, possiamo utilizzare la funzione `:rand.uniform/1` passando come argomento l'intervallo di numeri che vogliamo generare. Ad esempio, se vogliamo un numero compreso tra 1 e 10, possiamo scrivere così:

```Elixir
:rand.uniform(1..10) 
```

Questo genererà un numero casuale ogni volta che eseguiamo il codice.

Se invece vogliamo generare una lista di numeri casuali, possiamo utilizzare la funzione `Enum.map/2` per applicare `:rand.uniform/1` ad ogni elemento della lista. Ecco un esempio:

```Elixir
Enum.map(1..10, &rand.uniform/1)
```

Questo produrrà una lista di 10 numeri casuali.

## Approfondimento

Elixir utilizza l'algoritmo di rigenerazione Mersenne Twister per generare numeri casuali. Questo algoritmo è molto veloce e produce numeri casuali di alta qualità.

È importante notare che, se vogliamo generare numeri riproducibili, dobbiamo passare un seed alla funzione `:rand.uniform/2`. Il seed è essenzialmente una "chiave" che viene utilizzata dall'algoritmo per generare i numeri casuali. Utilizzando lo stesso seed, otterremo sempre la stessa sequenza di numeri casuali. Ad esempio:

```Elixir
:rand.uniform(1..10, 42)
```

Genererà sempre la stessa sequenza di numeri casuali ogni volta che viene eseguito, perché il seed è impostato su 42.

## Vedi Anche

- La documentazione ufficiale sulla generazione di numeri casuali in Elixir: https://hexdocs.pm/elixir/Random.html
- Un articolo sui diversi modi di generare numeri casuali in Elixir: https://culttt.com/2016/05/02/creating-randomness-in-elixir/