---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trovare la lunghezza di una stringa in Haskell - Un rapido Tutorial

## Che cos'è e perché?

Trovare la lunghezza di una stringa significa conteggiare quante volte si verificano caratteri in una stringa. I programmatori fanno questo per vari motivi, ad esempio per validare l'input dell'utente o per manipolare stringhe in modi specifici.

## Come fare:

In Haskell, la lunghezza di una stringa si trova usando la funzione `length` così:

```Haskell
lunghezza :: String -> Int
lunghezza stringa = length stringa
```

Ecco un esempio di utilizzo:

```Haskell
main :: IO ()
main = putStrLn(show(lunghezza "Hello, World!"))

-- Output: 13
```

## Approfondimento

Historicamente, la funzione `length` in Haskell è stata sempre molto efficiente, grazie all'implementazione "lazy" di Haskell. Essa conta solo i caratteri iniziali di una stringa finché non raggiunge la fine, invece di caricare l'intera stringa in memoria.

Un'alternativa a 'length' sarebbe usare una funzione ricorsiva personalizzata per contare la lunghezza di una stringa. Ma in generale, `length` è più efficiente e conveniente.

Qui sotto c'è un dettaglio di implementazione: `length` è definita in termini della funzione di piegamento `foldr`. Una funzione di piegamento è una funzione che "piega" una struttura dati in un valore, seguendo le regole specificate. Nel caso di `length`, la funzione di piegamento conta il numero di elementi in una lista (o in questo caso, una stringa).

## Vedi anche

1. [Haskell Documentation](https://www.haskell.org/documentation/)
2. [Learn You a Haskell for a Great Good](http://learnyouahaskell.com/)
3. [Real World Haskell](http://book.realworldhaskell.org/)