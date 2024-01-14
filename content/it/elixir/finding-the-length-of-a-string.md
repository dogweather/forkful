---
title:    "Elixir: Trova la lunghezza di una stringa"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Elixir, allora probabilmente hai già familiarità con il concetto di stringhe. Ma perché dovresti interessarti a trovare la lunghezza di una stringa? In realtà, questa è un'operazione comune nella programmazione, soprattutto quando si lavora con dati e input dell'utente.

## Come fare

Per trovare la lunghezza di una stringa in Elixir, possiamo utilizzare la funzione `String.length/1`. Prende un argomento, ovvero la stringa di cui vogliamo sapere la lunghezza, e restituisce il numero di caratteri presenti in quella stringa. Ecco un esempio di codice:

```Elixir
stringa = "Ciao mondo!"
lunghezza = String.length(stringa)
IO.puts("La lunghezza della stringa è #{lunghezza}") 
```

In questo caso, `lunghezza` sarà uguale a 12 poiché la stringa contiene 12 caratteri. L'output del codice sarà "La lunghezza della stringa è 12".

## Approfondimento

Ora che sappiamo come trovare la lunghezza di una stringa, potresti chiederti come funziona esattamente questa operazione. In realtà, Elixir utilizza i byte per rappresentare una stringa, quindi la lunghezza della stringa sarà determinata dal numero di byte presenti al suo interno. Tuttavia, anche se otteniamo la lunghezza basandoci sui byte, Elixir ha la capacità di gestire anche caratteri multi-byte come quelli utilizzati nelle lingue asiatiche.

Un altro concetto importante da tenere a mente è che la funzione `String.length/1` non tiene conto dei caratteri speciali come l'apostrofo o gli accenti. Ad esempio, se abbiamo una stringa come "l'amore è la chiave per la felicità", la lunghezza sarà ancora di 26 caratteri, nonostante la presenza dell'apostrofo. Questo può essere importante da considerare quando si lavora con input dell'utente e si vuole gestire correttamente la lunghezza dei dati.

Inoltre, se abbiamo bisogno di trovare la lunghezza di una lista di stringhe, possiamo utilizzare la funzione `Enum.map/2` per applicare la funzione `String.length/1` a ogni elemento della lista. Ad esempio:

```Elixir
lista = ["Ciao", "Mondo", "Elixir"]
lunghezza_lista = Enum.map(lista, fn x -> String.length(x) end)
IO.inspect(lunghzza_lista) 
```

L'output sarà una lista contente le lunghezze di ogni stringa presente nella lista originale.

## Vedi anche

- [Documentazione di Elixir sulla funzione String.length/1](https://hexdocs.pm/elixir/String.html#length/1)
- [Tutorial su Elixir su come trovare la lunghezza di una stringa](https://elixir-lang.org/getting-started/strings-sigils-and-char-lists.html#the-length-function)