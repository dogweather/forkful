---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa e Perche?

Cancellare caratteri corrispondenti a un modello in Haskell, significa eliminare tutti i caratteri di una stringa che soddisfano un criterio specifico. Questa operazione è molto utile quando si vuole pulire o formattare i dati di input.

## Come Fare:

Ecco un esempio di come si può eliminare i caratteri corrispondenti a un modello in Haskell:

```Haskell
import Data.Char
import Data.List

eliminaCaratteri :: String -> String
eliminaCaratteri = filter (not . (`elem` ['a', 'e', 'i', 'o', 'u']))

main = do
    print (eliminaCaratteri "ciao mondo")
```

In questo codice, `eliminaCaratteri` è una funzione che elimina tutte le vocali da una stringa. Nel `main`, chiamiamo `eliminaCaratteri` su "ciao mondo" e l'output sarà:

`"c mnd"`

## Approfondimento

Nel linguaggio di programmazione Haskell, la funzione di ordine superiore `filter` viene utilizzata per eliminare caratteri secondo un modello. Haskell ha introdotto la programmazione funzionale, una filosofia che mette in evidenza l'uso di funzioni pure e evita i cambiamenti dello stato o i dati mutabili.

Se preferisci un approccio diverso, potresti considerare l'uso di regex per combinare e sostituire stringhe. Tuttavia, tieni presente che le regex possono essere più complesse.

In termini di implementazione, `filter` in Haskell è implementato come una funzione ricorsiva che itera sulla lista di input e costruisce una nuova lista con solo gli elementi che soddisfano la condizione data.

## Approfondimenti Utili

Per ulteriori informazioni sul filtraggio in Haskell, qui ci sono alcune risorse:

3. [Corso online su Haskell](https://www.futurelearn.com/courses/functional-programming-haskell): un corso online che insegna le basi di Haskell e la programmazione funzionale.