---
title:                "Ottenere la data corrente"
html_title:           "Haskell: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
La data corrente è semplicemente la data di oggi. I programmatori spesso utilizzano questa funzionalità per tenere traccia del tempo o per stampare la data in applicazioni come calendari o pannelli di controllo.

## How to:
Per ottenere la data corrente in Haskell, possiamo utilizzare la funzione `getCurrentTime` del modulo `Data.Time`. Dopo aver importato il modulo, possiamo semplicemente chiamare questa funzione per ottenere un valore di tipo `UTCTime`, che rappresenta l'ora corrente in formato universale.

```Haskell
import Data.Time
getCurrentTime
```
Output: `2021-10-04 20:24:16.123456 UTC`

Possiamo anche convertire la data in un formato più leggibile utilizzando la funzione `formatTime` e specificando il formato desiderato come primo argomento.

```Haskell
import Data.Time
import Data.Time.Format
getCurrentTime >>= print . formatTime defaultTimeLocale "%d/%m/%Y"
```
Output: `04/10/2021`

## Deep Dive:
La funzione `getCurrentTime` è stata introdotta nella versione 4.8.0.0 di Haskell e fa parte del pacchetto standard `time`. Inoltre, esistono altre librerie come `haskell-time` e `time-locale-compat` che forniscono funzioni simili per lavorare con le date in Haskell. Tuttavia, è importante notare che la gestione del tempo e delle date può essere complicata e può portare a errori se non trattata correttamente.

## See Also:
- [Documentazione ufficiale di `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial su come lavorare con le date in Haskell](https://mitchellwrosen.github.io/lifting-applications-ch-24.html)