---
title:    "Haskell: Calcolare una data nel futuro o nel passato."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molte situazioni. Ad esempio, potresti voler sapere quale giorno della settimana cadrà il tuo compleanno in un determinato anno, o quando un dato evento si ripeterà in futuro. Inoltre, imparare a scrivere codice per calcolare date può aiutarti a sviluppare abilità di programmazione più avanzate.

## Come fare

Per calcolare una data nel futuro o nel passato in Haskell, è necessario utilizzare la biblioteca `Time`. Ecco un esempio di codice che calcola la data di oggi più 10 giorni e la stampa nel formato "DD/MM/YYYY":

```Haskell
import Data.Time

today = getCurrentTime
futureDate = fmap (addDays 10) today
formattedDate = fmap (formatTime defaultTimeLocale "%d/%m/%Y") futureDate
print formattedDate
```

L'uscita di questo codice sarà `31/01/2021`, se eseguito il 21 gennaio 2021. Possiamo anche calcolare una data nel passato utilizzando `addDays (-10)`.

## Approfondimento

Per calcolare una data nel futuro o nel passato, è importante comprendere come Haskell gestisce le date. In Haskell, le date sono rappresentate utilizzando il tipo `UTCTime`, che rappresenta un momento specifico nel tempo in UTC. L'aggiunta di giorni o altri valori a una data avviene utilizzando le funzioni `addDays` e `addUTCTime`, rispettivamente. Inoltre, il formato di visualizzazione delle date è gestito dalla funzione `formatTime`, che accetta un formato desiderato e una data da formattare.

## Vedi anche

- [Documentazione di Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Esempi di Codice per Calcolare Date in Haskell](https://www.organicdesign.co.nz/Calculating_dates_in_Haskell)