---
title:                "Haskell: Convertire una stringa in minuscolo"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Sempre più spesso, durante la scrittura di codice Haskell, ci si trova ad avere la necessità di convertire una stringa in lettere minuscole. Questo può essere utile per confrontare stringhe in modo case-insensitive, oppure per uniformare un input da parte dell'utente.

## Come Fare

Per convertire una stringa in lettere minuscole in Haskell, possiamo utilizzare la funzione "toLower" del modulo "Data.Char". Questa funzione accetta come parametro una singola lettera e la converte in minuscolo. Per applicare questa funzione a una stringa, possiamo utilizzare la funzione "map", che applica una determinata funzione ad ogni elemento di una lista.

```Haskell
import Data.Char

-- Definiamo una stringa di esempio 
stringaIniziale = "TiPiCa Italia"

-- Applichiamo la funzione "toLower" a ogni carattere della stringa 
stringaFinale = map toLower stringaIniziale 

-- Output: "tipica italia"
```

## Approfondimento

Ma come funziona esattamente la funzione "toLower"? In realtà, questa funzione è solamente una convenienza per la funzione "toLower" definita all'interno del modulo "Data.Char". Questa funzione accetta come parametro un carattere Unicode e lo converte in minuscolo, se possibile. In questo modo, la funzione "toLower" può essere utilizzata per qualsiasi tipo di carattere, non solo lettere dell'alfabeto, rendendola estremamente versatile.

## Vedi Anche

- [Documentazione della funzione "toLower"](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toLower)
- [Documentazione del modulo "Data.Char"](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)