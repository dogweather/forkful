---
title:                "Haskell: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è un'operazione comune nella programmazione di Haskell per diverse ragioni. Ad esempio, può essere utile per mostrare una data all'utente o per salvare una data in un file di testo.

## Come fare

Per convertire una data in una stringa in Haskell, possiamo utilizzare la funzione `show` seguita da una data del tipo `Data.DateTime`. Di seguito un esempio:

```Haskell
import Data.DateTime
 
main = do
  let date = fromGregorian 2020 5 31
  let dateString = show date
  putStrLn dateString
```

L'output di questo codice sarà `2020-05-31T00:00:00Z`, con il formato standard ISO 8601 per le date.

È anche possibile specificare un formato particolare utilizzando la funzione `formatDateTime` del modulo `Data.DateTime.Format`. Ad esempio:

```Haskell
import Data.DateTime
import Data.DateTime.Format

main = do
  let date = fromGregorian 2020 5 31
  let dateString = formatDateTime "%d/%m/%Y" date
  putStrLn dateString
```

In questo caso, l'output sarà `31/05/2020`.

## Approfondimento

La conversione di una data in una stringa coinvolge la gestione di diversi elementi, come il formato della data, il fuso orario e l'utilizzo di funzioni specifiche per gestire le date. Il modulo `Data.DateTime` fornisce diverse funzioni utili per lavorare con le date, oltre a gestire automaticamente il fuso orario locale.

Inoltre, ci sono molti altri moduli disponibili su Hackage, la piattaforma ufficiale per i pacchetti di Haskell, che offrono funzioni avanzate per la manipolazione e la conversione delle date.

## Vedi anche

- [Documentazione ufficiale Data.DateTime](https://hackage.haskell.org/package/datetime)
- [Pacchetto on Hackage per formattare le date](https://hackage.haskell.org/package/time-format)
- [Guida alla conversione delle date in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#pick-of-the-week--converting-dates)