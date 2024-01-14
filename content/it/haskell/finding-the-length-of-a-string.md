---
title:    "Haskell: Trovare la lunghezza di una stringa."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune durante la programmazione. Sapere come farlo in Haskell può aiutare a rendere il tuo codice più efficiente e leggibile.

## Come fare

Per trovare la lunghezza di una stringa in Haskell, puoi utilizzare la funzione predefinita `length`. Ad esempio:

```Haskell
length "ciao" -- output: 4
```

Se vuoi avere maggior controllo sul processo di calcolo della lunghezza, puoi scrivere la tua funzione utilizzando una ricorsione. Ecco un esempio:

```Haskell
length' :: String -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

length' "ciao" -- output: 4
```

## Approfondimento

Per capire meglio come funziona la funzione `length` in Haskell, dobbiamo guardare all'implementazione di base. La funzione `length` è definita come segue:

```Haskell
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
```

In questo caso, `[a]` rappresenta una lista di elementi di tipo generico `a`. La funzione utilizza la ricorsione per scorrere la lista e conta ogni elemento utilizzando 1 come base e sommando 1 ogni volta che viene trovato un nuovo elemento. Quando la lista è vuota, il conteggio termina restituendo 0.

## Vedi anche

- [Documentazione Hackage per la funzione `length`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#g:5)
- [Tutorial su come utilizzare la ricorsione in Haskell](https://www.tutorialspoint.com/haskell/haskell_recursion.htm)
- [Un esempio di utilizzo della funzione `length` in un programma Haskell](https://www.geeksforgeeks.org/haskell-length-function/)