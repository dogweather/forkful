---
title:                "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Spesso ci troviamo a dover lavorare con stringhe di testo in programmi Haskell. E una delle operazioni più comuni da eseguire su una stringa è la ricerca della sua lunghezza. Ma perché dovremmo preoccuparci di trovare la lunghezza di una stringa? Continua a leggere per scoprirlo!

## Come fare

Per trovare la lunghezza di una stringa in Haskell, possiamo utilizzare la funzione `length`. Questa funzione accetta una stringa come argomento e restituisce il numero di caratteri presenti nella stringa.

```Haskell
length "Ciao mondo!" 
-- Output: 11
```

In questo esempio, abbiamo utilizzato la funzione `length` per trovare la lunghezza della stringa "Ciao mondo!". Come puoi vedere, il risultato è 11 poiché la stringa è composta da 11 caratteri.

Ma cosa succede se vogliamo trovare la lunghezza di una stringa vuota? In tal caso, otterremo un risultato di 0.

```Haskell
length "" 
-- Output: 0
```

Abbiamo anche la possibilità di applicare la funzione `length` a una lista di stringhe invece che a una sola stringa.

```Haskell
length ["Ciao", "mondo", "!"]
-- Output: 3
```

## Approfondimento

In Haskell, la funzione `length` è definita ricorsivamente, il che significa che viene applicata ripetutamente fino a quando non viene raggiunto il caso base. Nel caso della stringa vuota, il caso base è 0, poiché una stringa vuota non ha alcun carattere. Nel caso di una stringa non vuota, la funzione viene applicata alla stringa privata del primo carattere e viene aggiunto 1 al risultato finale.

Inoltre, è importante notare che la funzione `length` è molto efficiente in quanto tiene traccia della lunghezza della stringa durante il processo di esecuzione del programma invece di doverla calcolare ogni volta.

## Vedi anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Funzioni di base in Haskell](https://www.haskell.org/onlinereport/standard-prelude.html)