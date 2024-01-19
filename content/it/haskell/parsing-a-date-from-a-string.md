---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché? 
L'analisi di una data da una stringa consiste nel trasformare una data rappresentata come una serie di caratteri in un valore di tipo data che può essere utilizzato in un programma. I programmatori lo fanno perché consente all'applicazione di interagire con le date in un modo più funzionale, come il confronto di date o il calcolo di intervalli di tempo.

## Come si fa:
In Haskell, possiamo utilizzare la libreria `Data.Time` per analizzare una data da una stringa. Ecco un esempio semplice:

```Haskell
import Data.Time

main :: IO ()
main = do
    let dataStr = "2021-01-01"
    let dataVal = parseTimeM True defaultTimeLocale "%Y-%m-%d" dataStr :: Maybe Day
    print dataVal
```

## Approfondimento
Nel contesto storico, l'elaborazione di stringhe di data è un problema antico nel campo della programmazione. Le librerie evolute come `Data.Time` in Haskell rendono il compito relativamente semplice oggi, ma la questione della gestione dei diversi formati di data, dei fusi orari e dell'ora legale rimane una sfida. Ci sono molte alternative per affrontare il problema, inclusa la scrittura di funzioni personalizzate per gestire specifici formati di data. Usando `defaultTimeLocale`, `Data.Time` può gestire comodamente molti formati comuni, ma può essere personalizzato per gestire formati di data insoliti. 

## Vedi Anche

- Documentazione ufficiale Haskell su [Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Post sul blog ["Parsing Time – the Haskell Way"](https://24daysindecember.net/2018/12/11/parsing-time-the-haskell-way/) che offre suggerimenti e trucchi utili nella gestione delle date in Haskell.