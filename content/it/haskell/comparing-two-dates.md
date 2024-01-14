---
title:    "Haskell: Confrontare due date"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date può essere utile in molte situazioni, come ad esempio nel calcolo della differenza tra due date, nell'ordinamento di eventi cronologici o nella gestione di dati temporali. Inoltre, conoscere come comparare date in Haskell può aiutare a comprendere meglio il concetto di tipi di dati e di funzioni.

## Come

Per confrontare due date in Haskell, è necessario prima di tutto importare il modulo `Data.Time`, che fornisce funzioni per la gestione del tempo. Successivamente, definire le due date da confrontare utilizzando il tipo di dato `Day`.

```Haskell
import Data.Time

-- Definizione di due date
data1 = fromGregorian 2020 10 5
data2 = fromGregorian 2020 10 10

-- Confronto delle due date
compareDates data1 data2
```

Il risultato del confronto sarà un valore di tipo `Ordering`, che può essere `LT` (lower than), `GT` (greater than) o `EQ` (equal). Per poter utilizzare direttamente il valore comparato, è possibile utilizzare la funzione `toEnum` per trasformarlo in un valore `Int`.

## Deep Dive

In Haskell, le date sono rappresentate come valori immutabili e sono gestite tramite funzioni specifiche del modulo `Data.Time`. È importante notare che il tipo di dato `Day` rappresenta solo una data e non un'ora specifica. Se si desidera confrontare date e orari, allora è necessario utilizzare il tipo di dato `UTCTime`.

Un altro aspetto importante è la possibilità di formattare le date in diversi formati utilizzando la funzione `formatTime`, che richiede un formato di output e un valore di tipo `Day` come argomenti.

```Haskell
-- Formattazione di una data
formattedDate = formatTime defaultTimeLocale "%d/%m/%Y" data1
```

## Vedi anche

- [Documentazione di Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial su Data.Time](https://www.haskell.org/haskellwiki/Time)
- [Guida sulle funzioni di confronto in Haskell](https://wiki.haskell.org/Comparison_functions)