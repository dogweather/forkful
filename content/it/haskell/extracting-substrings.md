---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

**## Che cos'è e perché?**

L'estrazione di sottostringhe è l'operazione che permette di ottenere una parte di una stringa, partendo da una posizione e una lunghezza specificate. Questa operazione è fondamentale in programmazione per manipolare dati testuali, per esempio per dividere stringhe più grandi in parti più piccole, o per isolare informazioni specifiche all'interno di un testo.

**## Come fare:**

In Haskell, è possibile estrarre una sottostringa usando la funzione `drop` e `take`. `drop n` rimuove i primi `n` caratteri da una stringa, mentre `take m` preleva i primi `m` caratteri da una stringa.

```Haskell
substring :: Int -> Int -> String -> String
substring start len = take len . drop start
```

Ecco un esempio di utilizzo:

```Haskell
main = do
  let s = "Buongiorno, mondo!"
  print $ substring 0 9 s  -- Output: "Buongiorno"
  print $ substring 11 5 s -- Output: "mondo"
```
Con questo codice, stiamo estraendo le sottostringhe "Buongiorno" e "mondo" dalla stringa "Buongiorno, mondo!".

**## Approfondimento:**

L'estrazione delle sottostringhe è un concetto vecchio quanto il concetto di stringa stessa e viene utilizzato in quasi tutti i linguaggi di programmazione. Mentre alcuni linguaggi hanno funzioni integrate per questa operazione, in Haskell dobbiamo creare una funzione personalizzata utilizzando `drop` e `take`.

Un'alternativa a questa funzione sarebbe l'utilizzo delle funzioni `splitAt` o `slice`, ma non sono tanto flessibili quanto la combinazione di `drop` e `take`.

In termini di prestazioni, l'operazione `drop n` può essere costosa se `n` è grande, in quanto deve scorrere la lista fino all’`n`-esimo elemento. Per il contrario, l'operazione `take m` è effettivamente costante rispetto alla lunghezza della stringa originale.

**## Vedi anche:**

Per ulteriori approfondimenti su come lavorare con le stringhe in Haskell, consulta le seguenti risorse:

1. [Lavorare con le Stringhe in Haskell](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
3. [Hoogle: Una Ricerca API per Haskell](https://www.haskell.org/hoogle/)