---
date: 2024-01-19
description: "How to: Um CSV-Dateien in Haskell zu verarbeiten, nutzen wir die Bibliothek\
  \ `cassava`. Installiere `cassava` mit `cabal install cassava` oder `stack add\u2026"
lastmod: '2024-03-13T22:44:53.953086-06:00'
model: unknown
summary: Um CSV-Dateien in Haskell zu verarbeiten, nutzen wir die Bibliothek `cassava`.
title: Arbeiten mit CSV-Dateien
weight: 37
---

## How to:
Um CSV-Dateien in Haskell zu verarbeiten, nutzen wir die Bibliothek `cassava`. Installiere `cassava` mit `cabal install cassava` oder `stack add cassava`. Hier ein einfaches Beispiel:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

-- Definiere den Typ für unsere Daten
type Person = (String, Int, String)

-- Lese und schreibe eine CSV-Datei
main :: IO ()
main = do
    csvData <- BL.readFile "personen.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, alter, stadt) ->
            putStrLn $ name ++ " ist " ++ show alter ++ " Jahre alt und kommt aus " ++ stadt

    let personen :: [Person]
        personen = [("Anna", 28, "Berlin"), ("Lukas", 35, "München"), ("Julia", 24, "Köln")]
    
    BL.writeFile "neue_personen.csv" $ encode personen
```

Sample `personen.csv`:
```
Anna,28,Berlin
Lukas,35,München
Julia,24,Köln
```

Beim Ausführen werden die Daten gelesen und wie folgt ausgegeben:
```
Anna ist 28 Jahre alt und kommt aus Berlin
Lukas ist 35 Jahre alt und kommt aus München
Julia ist 24 Jahre alt und kommt aus Köln
```

## Deep Dive
CSV-Dateien sind ein Relikt aus den frühen EDV-Tagen, gedacht für den simplen Datenaustausch zwischen Programmen und Systemen. Alternativen zu CSV sind beispielsweise JSON, XML und YAML – sie bieten komplexere Strukturen und Datenmodelle. Die `cassava`-Bibliothek in Haskell abstrahiert CSV-Handhabung über Typklassen wie `FromRecord` und `ToRecord`, die das Parsen und Serialisieren von Daten erleichtern.

## See Also:
- `cassava` Bibliothek auf Hackage: [https://hackage.haskell.org/package/cassava](https://hackage.haskell.org/package/cassava)
