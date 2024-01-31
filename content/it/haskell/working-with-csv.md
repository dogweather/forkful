---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Il CSV, ovvero Comma-Separated Values, è un formato di file usato per memorizzare i dati in forma tabellare semplice. I programmatori lo usano spesso per importare ed esportare dati da database, fogli elettronici o servizi web per la sua leggibilità e semplicità.

## How to:
In Haskell, usiamo librerie come `cassava` per lavorare con CSV. Ecco un esempio semplice per leggere un file CSV.

```Haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

main :: IO ()
main = do
    csvData <- BL.readFile "dati.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (nome, eta) ->
            putStrLn $ nome ++ " ha " ++ show eta ++ " anni"
```

Assumendo `dati.csv` sia:

```
Giovanni,30
Francesca,28
```

L'output sarà:

```
Giovanni ha 30 anni
Francesca ha 28 anni
```

## Deep Dive
Il CSV non è un formato standardizzato, cosa che può portare a variazioni di implementazione. Nasce negli anni ‘70 come soluzione semplice per database. Alternativamente, ci sono formati come JSON o XML, più strutturati ma meno leggibili dall’uomo. In Haskell, lavorare con CSV si appoggia al laziness del linguaggio per gestire grandi quantità di dati in modo efficiente.

## See Also
- Documentazione di `cassava`: [http://hackage.haskell.org/package/cassava](http://hackage.haskell.org/package/cassava)
- Haskell Programming from first principles: [http://haskellbook.com](http://haskellbook.com)
