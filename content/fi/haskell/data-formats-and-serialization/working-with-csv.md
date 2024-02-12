---
title:                "Työskentely CSV:n kanssa"
date:                  2024-02-03T19:19:53.199380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely CSV:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

CSV-tiedostojen (pilkuilla erotetut arvot) käsittely sisältää tiedostojen jäsentämisen ja luomisen, jotka tallentavat taulukkomuotoista dataa yksinkertaisessa, tekstipohjaisessa muodossa. Ohjelmoijat osallistuvat usein tähän tehtävään tehokkaasti tuodakseen tai viedäkseen dataa laskentataulukoista, tietokannoista, tai helpottaakseen datan vaihtoa eri ohjelmien välillä.

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
