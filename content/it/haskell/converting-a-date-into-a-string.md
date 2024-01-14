---
title:    "Haskell: Convertire una data in una stringa"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è un'azione molto comune durante la programmazione in Haskell. Questo può essere necessario per visualizzare le date in un formato specifico o per eseguire operazioni di confronto.

## Come Fare

```Haskell
import Data.Time.Format

-- Definizione della data
let data = fromGregorian 2020 10 15

-- Conversione in una stringa usando il formato "anno/mese/giorno"
let stringaData = formatTime defaultTimeLocale "%Y/%m/%d" data

-- Output: "2020/10/15"
```

## Approfondimento

La funzione `formatTime` fa parte del modulo `Data.Time.Format` e permette di convertire una data nel formato di stringa desiderato. Inoltre, è importante specificare anche il `defaultTimeLocale`, che indica la localizzazione di default per il formato della data.

Altri formati comuni sono:

- `%A, %B %e, %Y`: "giovedì, ottobre 15, 2020"
- `%m/%d/%y`: "10/15/20"
- `%H:%M:%S`: "14:30:45"

Per ulteriori informazioni sulle opzioni di formattazione della data, si consiglia di consultare la documentazione ufficiale di Haskell.

## Vedi Anche

- [Documentazione ufficiale di Haskell sulle date](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Esempi pratici di conversione di date in stringhe in Haskell](https://www.geeksforgeeks.org/haskell-date-string/?ref=rp)