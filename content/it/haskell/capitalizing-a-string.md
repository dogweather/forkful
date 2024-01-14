---
title:    "Haskell: Capitalizzare una stringa"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché Capitalizzare una Stringa?

Capitalizzare una stringa può essere importante quando si vuole dare enfasi a una certa parte di testo o solo per motivi di estetica. In Haskell, ci sono diverse opzioni per capitalizzare una stringa e in questo articolo esploreremo le diverse possibilità.

## Come Capitalizzare una Stringa in Haskell

Per capitalizzare una stringa in Haskell, è possibile utilizzare la funzione `toUpper` del modulo `Data.Char`. Vediamo un esempio:

```Haskell
import Data.Char

-- Definiamo una funzione che prende una stringa e la restituisce capitalizzata
capitalizza :: String -> String
capitalizza str = map toUpper str

-- Utilizziamo la funzione con una stringa di esempio
capitalizza "ciao a tutti" -- "CIAO A TUTTI"
```

Si noti che `map` applica la funzione `toUpper` a ogni carattere della stringa, restituendo così una nuova stringa con tutti i caratteri maiuscoli.

In alternativa, si può utilizzare anche la funzione `toUpper` direttamente sulla stringa:

```Haskell
import Data.Char

-- Utilizziamo la funzione `toUpper` sulla stringa di esempio
map toUpper "ciao a tutti" -- "CIAO A TUTTI"
```

Un'altra opzione è utilizzare la libreria `Data.Text`, che offre molte funzioni per lavorare con stringhe di testo. Vediamo un esempio utilizzando la funzione `toUpper` del modulo `Data.Text`:

```Haskell
import Data.Text as T

-- Utilizziamo la funzione `toUpper` sulla stringa di esempio
T.toUpper "ciao a tutti" -- "CIAO A TUTTI"
```

## Approfondimento sulla Capitalizzazione di una Stringa

In Haskell, le stringhe sono in realtà liste di caratteri. Questo significa che, quando si applica la funzione `map` o `toUpper` a una stringa, in realtà si sta mappando una lista di caratteri. Per questo motivo, è importante essere consapevoli della dimensione della stringa su cui si sta lavorando e della complessità computazionale della funzione utilizzata.

Inoltre, è importante ricordare che la funzione `toUpper` è dipendente dalla lingua in cui si sta lavorando. Ad esempio, in italiano la lettera "i" maiuscola è "I", mentre in tedesco è "Ì". Pertanto, se si lavora con testo multilingue, è importante considerare queste differenze linguistiche.

## Vedi anche

- [Data.Char - HaskellDoc](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Data.Text - HaskellDoc](https://hackage.haskell.org/package/text/docs/Data-Text.html)