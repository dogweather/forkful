---
title:                "Haskell: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui qualcuno potrebbe voler lavorare con file CSV utilizzando Haskell. Potrebbe essere necessario analizzare grandi quantità di dati, creare strumenti di import/export o semplicemente imparare una nuova tecnologia interessante.

## Come Fare

Per lavorare con file CSV in Haskell, è necessario importare il modulo `Data.Csv` e utilizzare la funzione `decode` per analizzare il file. Ad esempio, supponiamo di avere un file CSV con i seguenti dati:

```Haskell
nome,eta,città
Mario,32,Roma
Giulia,26,Milano
```

Possiamo utilizzare il seguente codice per leggere e stampare i dati:

```Haskell
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  csvData <- BL.readFile "file.csv"
  case decode NoHeader csvData of
    Left err -> print err
    Right rows -> mapM_ print (rows :: [Record])
```

La funzione `decode` accetta come parametri il tipo di intestazione del file CSV e i dati del file stesso. Se tutto va bene, restituirà una lista di valori del tipo `Record`, che può essere stampata utilizzando la funzione `print`.

L'output generato da questo codice sarà il seguente:

```
["Mario", "32", "Roma"]
["Giulia", "26", "Milano"]
```

## Approfondimento

Il modulo `Data.Csv` offre molte funzionalità per lavorare con i file CSV in Haskell. Ad esempio, è possibile specificare il tipo di dati dei campi del file CSV e la funzione `decode` restituirà una lista di valori del tipo desiderato. Inoltre, è possibile utilizzare la funzione `encode` per generare un file CSV a partire da una lista di valori.

Per ulteriori informazioni su come lavorare con file CSV in Haskell, si consigliano i seguenti link:

* [Documentazione ufficiale del modulo `Data.Csv`](https://hackage.haskell.org/package/cassava/docs/Data-Csv.html)
* [Tutorial su come utilizzare il modulo `Data.Csv`](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_CSV_processing)
* [Esempi di codice per lavorare con file CSV in Haskell](https://github.com/haskelling/haskell-companies/blob/master/src/exercises-fix-provider.hs)

## Vedi Anche

* [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
* [Comunità italiana di Haskell](https://it-it.facebook.com/groups/haskell.it/)
* [Lista dei tutorial di Haskell disponibili online](https://wiki.haskell.org/Tutorials)