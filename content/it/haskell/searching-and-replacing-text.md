---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Haskell - Ricerca e Sostituzione del Testo

## Cos'è & Perché?
La ricerca e la sostituzione del testo è l'operazione di identificare stringhe specifiche in un testo e sostituirle con altre stringhe. È fondamentale nella programmazione per manipolare i dati e per il raffinamento degli algoritmi.

## Come si fa:
Ecco un esempio anche più semplice di come fare la ricerca e la sostituzione di testo in Haskell.

```Haskell
import Data.List.Utils

main = do
    let str = "Ciao, mondo!"
    putStrLn $ replace "mondo" "Haskell" str
```
Quando eseguiamo questo codice, l'output sarà:

```
Ciao, Haskell!
```
L'operazione di sostituzione è realizzata dalla funzione `replace` nel modulo `Data.List.Utils`.

## Approfondimento
Nel contesto storico, la ricerca e la sostituzione del testo è stata una funzione chiave delle prime tecnologie di editing del testo. In Haskell, esistono molte funzioni di libreria standard per manipolare le stringhe, tra cui ricerca e sostituzione.

Ci sono alternative alla funzione `replace`, ad esempio, possiamo implementare manualmente una funzione di ricerca e sostituzione con l'aiuto di altre funzioni Haskell. Ecco un esempio:

```Haskell
mio_replace :: Eq a => [a] -> [a] -> [a] -> [a]
mio_replace cercato sostituisci = unfoldr (\b -> if null b then Nothing else Just (go b))
    where
        go s
           | cercato `isPrefixOf` s = (sostituisci, drop (length cercato) s)
           | otherwise = ([head s], tail s)
```           
La funzione `mio_replace` definisce un metodo personalizzato per effettuare la ricerca e sostituzione nel modulo `Data.List`.

## Vedi anche
Per ulteriori informazioni sulla programmazione in Haskell, consulta:

1. "Learn You a Haskell for Great Good!": http://learnyouahaskell.com/chapters

2. Servizi di librerie Haskell: http://hackage.haskell.org/

3. Haskell Language: https://www.haskell.org/