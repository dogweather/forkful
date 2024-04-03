---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:53.199380-07:00
description: "Miten: Haskellissa CSV-tiedostojen k\xE4sittelyn voi saavuttaa k\xE4\
  ytt\xE4m\xE4ll\xE4 `cassava`-kirjastoa, joka on yksi suosituimmista kolmannen osapuolen\
  \ kirjastoista\u2026"
lastmod: '2024-03-13T22:44:56.636770-06:00'
model: gpt-4-0125-preview
summary: "Haskellissa CSV-tiedostojen k\xE4sittelyn voi saavuttaa k\xE4ytt\xE4m\xE4\
  ll\xE4 `cassava`-kirjastoa, joka on yksi suosituimmista kolmannen osapuolen kirjastoista\
  \ t\xE4h\xE4n tarkoitukseen."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Miten:
Haskellissa CSV-tiedostojen käsittelyn voi saavuttaa käyttämällä `cassava`-kirjastoa, joka on yksi suosituimmista kolmannen osapuolen kirjastoista tähän tarkoitukseen. Alla on esimerkkejä, jotka esittelevät, kuinka lukea ja kirjoittaa CSV-tiedostoihin käyttäen `cassava`a.

**1. CSV-tiedoston lukeminen:**

Varmista ensin, että sinulla on `cassava` asennettuna lisäämällä se projektisi cabal-tiedostoon tai käyttämällä Stackia.

Tässä on yksinkertainen esimerkki CSV-tiedoston lukemiseen ja jokaisen tietueen tulostamiseen. Oletamme, että CSV-tiedostossa on kaksi saraketta: nimi ja ikä.

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
            putStrLn $ name ++ " on " ++ show (age :: Int) ++ " vuotta vanha."
```

Olettaen että `people.csv` sisältää:
```
John,30
Jane,25
```
Tuloste on:
```
John on 30 vuotta vanha.
Jane on 25 vuotta vanha.
```

**2. CSV-tiedoston kirjoittaminen:**

CSV-tiedoston luomiseksi voit käyttää `encode`-funktiota `cassava`sta.

Tässä on, kuinka voisit kirjoittaa listan tietueita CSV-tiedostoon:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Tämän ohjelman suorittamisen jälkeen `output.csv` sisältää:

```
John,30
Jane,25
```

Tämä tiivis johdatus CSV-tiedostojen käsittelyyn Haskellissa käyttäen `cassava`-kirjastoa esittelee, kuinka lukea ja kirjoittaa CSV-tiedostoihin, tehden datan käsittelytehtävistä helpommin lähestyttäviä niille, jotka ovat uusia kielessä.
