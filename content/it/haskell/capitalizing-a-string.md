---
title:    "Haskell: Capitalizzando una stringa"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Spesso nei nostri programmi, dobbiamo manipolare delle stringhe. Una delle operazioni più comuni è quella di convertire la prima lettera di una stringa in maiuscolo. In questo articolo, vi spiegherò come fare questo in modo semplice utilizzando il linguaggio di programmazione funzionale Haskell.

## Come Fare

Per convertire la prima lettera di una stringa in maiuscolo in Haskell, possiamo utilizzare la funzione predefinita "toUpper" del modulo "Data.Char". Ecco un esempio di codice:

```Haskell
import Data.Char

toUpperFirst :: String -> String
toUpperFirst s = toUpper (head s) : tail s
-- In questo esempio, utilizziamo la funzione "head", che restituisce il primo elemento di una lista,
-- e la funzione "tail", che restituisce tutti gli elementi tranne il primo.
-- In questo modo, possiamo convertire solo la prima lettera della stringa in maiuscolo,
-- mantenendo invariato il resto della stringa.

main :: IO ()
main = do
    let str = "ciao a tutti"
    putStrLn "Stringa originale:"
    putStrLn str
    let strUp = toUpperFirst str
    putStrLn "Stringa con prima lettera maiuscola:"
    putStrLn strUp
```
L'output di questo codice sarà:

```Haskell
Stringa originale:
ciao a tutti
Stringa con prima lettera maiuscola:
Ciao a tutti
```

## Approfondimento

In Haskell, le stringhe sono semplicemente delle liste di caratteri. Quindi, nel nostro esempio, la funzione "toUpperFirst" prende in input una lista di caratteri (cioè una stringa), e restituisce una nuova lista di caratteri con la prima lettera convertita in maiuscolo. Possiamo anche utilizzare questa funzione per convertire la prima lettera di ogni parola di una stringa in maiuscolo, come nel seguente esempio:

```Haskell
import Data.Char

toUpperWords :: String -> String
toUpperWords s = unwords (map toUpperFirst (words s))
-- La funzione "words" divide una stringa in parole, mentre la funzione "unwords" unisce una lista di stringhe
-- in un'unica stringa, separando ogni elemento con uno spazio.
-- Utilizziamo anche la funzione "map" per applicare la funzione "toUpperFirst" a ogni parola della stringa.

main :: IO ()
main = do
    let str = "ciao a tutti"
    putStrLn "Stringa originale:"
    putStrLn str
    let strUp = toUpperWords str
    putStrLn "Stringa con prime lettere maiuscole:"
    putStrLn strUp
```
L'output di questo codice sarà:

```Haskell
Stringa originale:
ciao a tutti
Stringa con prime lettere maiuscole:
Ciao A Tutti
```

## Vedi Anche

- [Documentazione di Haskell](https://www.haskell.org/documentation/)
- [Funzioni predefinite di Haskell](https://www.haskell.org/onlinereport/standard-prelude.html)