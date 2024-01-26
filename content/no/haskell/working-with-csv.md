---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV (Comma-Separated Values) gjør det lett å lagre og dele tabell data. Programmerere bruker det fordi det er enkelt, støttet av mange verktøy og lett å integrere med andre systemer.

## Hvordan:
Haskell gjør det enkelt å jobbe med CSV ved hjelp av biblioteker som `cassava`. Her er et grundig eksempel:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Anta at vi har en CSV fil 'data.csv' med følgende innhold:
-- Navn,Alder,Yrke
-- Ola,34,Snekker
-- Kari,42,Arkitekt

main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (navn, alder, yrke) ->
            putStrLn $ navn ++ " er " ++ show alder ++ " år gammel og jobber som " ++ yrke

type Person = (String, Int, String)
instance FromRecord Person where
    parseRecord v
        | V.length v == 3 = (,,) <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise       = mzero
```
Kjøret av koden vil produsere følgende:

```
Ola er 34 år gammel og jobber som Snekker
Kari er 42 år gammel og jobber som Arkitekt
```

## Deep Dive
CSV-formatet har vært rundt siden 1970-tallet og er fortsatt populært på grunn av sin enkelhet. Alternativer som JSON og XML gir mer struktur, men CSV er ofte raskere både å lese og skrive. Når det gjelder implementasjon, er parsing av CSV i Haskell effektivt – biblioteker som `cassava` håndterer hjørnetilfeller og effektivitet godt.

## Se Også
- Hackage `cassava` pakken: [https://hackage.haskell.org/package/cassava](https://hackage.haskell.org/package/cassava)
- En dybdegående tutorial på file I/O i Haskell: [https://wiki.haskell.org/IO_inside](https://wiki.haskell.org/IO_inside)
