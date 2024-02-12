---
title:                "Calcolo di una data futura o passata"
aliases:
- it/haskell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:21.252897-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calcolare una data nel futuro o nel passato è una pratica comune in programmazione, utile per gestire eventi, scadenze e funzionalità legate al tempo. I programmatori lo fanno per tracciare periodi di tempo, programmare compiti futuri o analizzare dati storici.

## How to:
In Haskell, si può usare la libreria `time` per maneggiare date. Ecco un esempio per calcolare una data 10 giorni nel futuro dalla data corrente:

```Haskell
import Data.Time
import Data.Time.Calendar

addDaysExample :: IO ()
addDaysExample = do
    today <- utctDay <$> getCurrentTime
    let tenDaysLater = addDays 10 today
    putStrLn $ "Oggi è: " ++ show today
    putStrLn $ "Tra 10 giorni sarà: " ++ show tenDaysLater

-- Esempio di output:
-- Oggi è: 2023-04-21
-- Tra 10 giorni sarà: 2023-05-01
```

Per andare indietro nel tempo, usiamo `addDays` con un numero negativo:

```Haskell
subtractDaysExample :: IO ()
subtractDaysExample = do
    today <- utctDay <$> getCurrentTime
    let tenDaysBefore = addDays (-10) today
    putStrLn $ "Oggi è: " ++ show today
    putStrLn $ "10 giorni fa era: " ++ show tenDaysBefore

-- Esempio di output:
-- Oggi è: 2023-04-21
-- 10 giorni fa era: 2023-04-11
```

## Deep Dive:
Historically, dates in programming had to account for various calendars and time zones, a complexity Haskell's `time` library manages well. Alternatives include the older `Data.Time.Calendar` module or external libraries like `thyme`.

La `time` library segue gli standard UTC e può gestire anche i secondi bisestili. Usa tipi di dati immutabili che aiutano a prevenire errori comuni in altri linguaggi, come le modifiche non intenzionali dello stato.

Per quanto riguarda l'implementazione, Haskell appoggia la semantica funzionale. Funzioni come `addDays` sono pure e non hanno effetti collaterali, rendendole componibili e affidabili per calcoli di tempo ripetibili.

## See Also:
Per maggiori informazioni, collegatevi alle seguenti risorse:
- [Haskell Time Library Documentation](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time.html)
- [ZonedTime in Haskell for Timezone Handling](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-LocalTime.html)
