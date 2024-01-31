---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:36:32.425244-07:00
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Analizzare una data da una stringa significa convertire testo in un formato data riconoscibile dal programma. Questo processo è essenziale per leggere e manipolare date inserite dall'utente o estratte da documenti e database.

## How to:
Utilizziamo la libreria `time` per parse le date. Ecco un esempio:

```haskell
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar

parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

-- Uso
main :: IO ()
main = case parseDate "2023-03-25" of
  Just day -> putStrLn $ "Data analizzata: " ++ show day
  Nothing -> putStrLn "Formato data non valido."
```

Output:

```
Data analizzata: 2023-03-25
```

## Deep Dive
La gestione delle date in Haskell ha le sue radici nel modulo `Data.Time`, che fornisce funzionalità per lavorare con tempo e date. 

Altre librerie, come `time-parsers`, offrono funzioni di parsing più avanzate. 

Dal punto di vista implementativo, parsing significa interpretare una stringa seguendo un formato specifico, come `"%Y-%m-%d"` che sta per anno-mese-giorno, e convertirla in un tipo `Day`.

## See Also
- Documentazione `time`: http://hackage.haskell.org/package/time
- Tutorial su date e tempo in Haskell: https://www.haskell.org/tutorial/dates.html
- Pacchetto `time-parsers` su Hackage: http://hackage.haskell.org/package/time-parsers
