---
title:                "Maiuscolare una stringa"
html_title:           "Haskell: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto la necessità di trasformare una stringa in maiuscolo? Se sì, allora hai bisogno di conoscere il concetto di capitalizzazione in Haskell! Continua a leggere per scoprire come farlo.

## Come Fare

Per capitalizzare una stringa in Haskell, è necessario utilizzare la funzione `toUpper` del modulo `Data.Char`. Ecco un esempio di codice per farlo:

```Haskell
import Data.Char

capitalizza :: String -> String  -- definizione della funzione 
capitalizza str = map toUpper str  -- utilizzo della funzione toUpper e della funzione map per applicarla a ogni carattere della stringa
```

Esempio di output:

```Haskell
capitalizza "haskell"  -- chiamata alla funzione 
"HASKELL"
```

## Approfondimento

La funzione `map` è un'operazione molto importante in Haskell. Permette di applicare una funzione a ogni elemento di una lista senza bisogno di effettuare un ciclo. Questo significa che possiamo utilizzare la funzione `map` anche per trasformare ogni singolo carattere di una stringa.

Un'altra cosa importante da notare è che la funzione `toUpper` non modifica la stringa originale, ma ne restituisce una nuova con tutti i caratteri trasformati in maiuscolo. Questo perché in Haskell le stringhe sono immutabili, il che significa che non possono essere modificate una volta create.

## Vedi Anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Tutorial di Haskell su Codecademy](https://www.codecademy.com/learn/learn-haskell)