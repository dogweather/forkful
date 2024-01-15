---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Haskell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato è un'attività comune nella programmazione per la gestione di eventi e scadenze. Con Haskell, è possibile gestire le date in modo efficiente e preciso grazie alla sua tipizzazione forte e funzioni integrate per la manipolazione delle date.

## Come fare

Per prima cosa, importa il modulo `Data.Time` per accedere alle funzioni di gestione del tempo. Quindi, puoi utilizzare la funzione `addDays` per aggiungere un numero di giorni a una data esistente.

```Haskell
import Data.Time

addDays :: Integer -> Day -> Day
```

Ad esempio, se vuoi calcolare la data tra 25 giorni, puoi scrivere il seguente codice:

```Haskell
import Data.Time

main = do
  let oggi = fromGregorian 2021 8 25   -- 25 agosto 2021
      futuraData = addDays 25 oggi     -- 19 settembre 2021

  print $ show futuraData

-- Output: 2021-09-19
```

In questo esempio, stiamo impostando una data di partenza utilizzando la funzione `fromGregorian` che accetta gli argomenti anno, mese e giorno. Quindi, stiamo utilizzando la funzione `addDays` per aggiungere 25 giorni alla data di partenza e ottenere la data nel futuro. Infine, utilizziamo la funzione `show` per convertire il valore in una stringa e quindi stampare il risultato.

## Approfondimento

Oltre alla funzione `addDays`, Haskell offre molte altre funzioni per la manipolazione delle date. Ad esempio, puoi utilizzare la funzione `diffDays` per calcolare la differenza in giorni tra due date.

```Haskell
import Data.Time

diffDays :: Day -> Day -> Integer
```

Puoi anche utilizzare la funzione `getCurrentTime` per ottenere la data e l'ora correnti del sistema, e utilizzare la funzione `formatTime` per formattare le date in diversi modi.

```Haskell
import Data.Time
import Data.Time.Format

main = do
  current <- getCurrentTime
  let formato = "%d %b %Y %T"  -- 25 ago 2021 20:30:05
      dataFormattata = formatTime defaultTimeLocale formato current

  print $ show dataFormattata

-- Output: "25 ago 2021 20:30:05"
```

Esplora la documentazione di Haskell per ulteriori informazioni sulle funzioni di gestione delle date e sperimenta con diversi formati per le date.

## Vedi anche

- [Haskell Date and Time Documentation](https://downloads.haskell.org/~ghc/8.8.3/docs/html/libraries/time-1.9.3/Data-Time.html)
- [Tutorialspoint Haskell Date and Time](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)
- [Learn You a Haskell - Time to look at some dates](http://learnyouahaskell.com/input-and-output#files-and-streams)