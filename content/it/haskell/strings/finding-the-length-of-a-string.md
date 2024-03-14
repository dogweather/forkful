---
date: 2024-01-20 17:47:31.256558-07:00
description: "Calcolare la lunghezza di una stringa significa contare quanti caratteri\
  \ contiene. I programmatori lo fanno per validare input, gestire formati di testo\
  \ o\u2026"
lastmod: '2024-03-13T22:44:43.466398-06:00'
model: gpt-4-1106-preview
summary: "Calcolare la lunghezza di una stringa significa contare quanti caratteri\
  \ contiene. I programmatori lo fanno per validare input, gestire formati di testo\
  \ o\u2026"
title: Trovare la lunghezza di una stringa
---

{{< edit_this_page >}}

## What & Why?
Calcolare la lunghezza di una stringa significa contare quanti caratteri contiene. I programmatori lo fanno per validare input, gestire formati di testo o per altre logiche specifiche.

## How to:
In Haskell, usi `length` per trovare la lunghezza di una stringa. Ecco un esempio:
```Haskell
lunghezza :: String -> Int
lunghezza = length

main :: IO ()
main = print (lunghezza "Ciao Mondo") -- Output sarà 10
```

## Deep Dive
La funzione `length` è parte del Prelude di Haskell, il modulo base carico per default. Storicamente, `length` è uno strumento fondamentale, semplice ma versatile. Ci sono alternative, come la ricorsione o fold, ma `length` è standard. Internamente, `length` itera sulla lista (una stringa in Haskell è una lista di caratteri) e conta gli elementi. Non è super efficiente con liste lunghe perché deve scorrerle completamente.

## See Also
Per approfondire, esplora la documentazione ufficiale di Haskell:
- Haskell Prelude: [https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:length](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:length)

Consulta anche:
- Learn You a Haskell for Great Good! An Introduction to Haskell via example: [http://learnyouahaskell.com/chapters](http://learnyouahaskell.com/chapters)
