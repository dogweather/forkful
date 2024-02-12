---
title:                "Lavorare con i CSV"
aliases:
- /it/haskell/working-with-csv/
date:                  2024-02-03T19:19:47.127903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con i CSV (Valori Separati da Virgola) implica l'analisi e la generazione di file che memorizzano dati tabellari in un formato semplice e basato su testo. I programmatori si impegnano spesso in questo compito per importare o esportare dati in modo efficiente da fogli di calcolo, database, o per facilitare lo scambio di dati tra diversi programmi.

## Come fare:

In Haskell, la gestione dei file CSV può essere realizzata utilizzando la libreria `cassava`, una delle più popolari librerie di terze parti per questo scopo. Di seguito sono riportati esempi che mostrano come leggere e scrivere file CSV utilizzando `cassava`.

**1. Leggere un file CSV:**

Prima di tutto, assicurati di avere `cassava` installato aggiungendolo al file cabal del tuo progetto o utilizzando Stack.

Ecco un semplice esempio per leggere un file CSV e stampare ogni record. Si assume che il file CSV abbia due colonne: nome e età.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " ha " ++ show (age :: Int) ++ " anni."
```

Assumendo che `people.csv` contenga:
```
John,30
Jane,25
```
L'output sarà:
```
John ha 30 anni.
Jane ha 25 anni.
```

**2. Scrivere un file CSV:**

Per creare un file CSV, puoi usare la funzione `encode` di `cassava`.

Ecco come potresti scrivere una lista di record in un file CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Dopo aver eseguito questo programma, `output.csv` conterrà:

```
John,30
Jane,25
```

Questa breve introduzione al lavoro con file CSV in Haskell utilizzando la libreria `cassava` dimostra come leggere e scrivere file CSV, rendendo i compiti di manipolazione dei dati più accessibili per chi è nuovo al linguaggio.
