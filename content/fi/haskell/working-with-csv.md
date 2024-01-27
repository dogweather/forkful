---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
CSV on datan tallennusmuoto, jota käytetään taulukollisen tiedon tallentamiseen tekstimuodossa. Ohjelmoijat käyttävät sitä, koska se on yksinkertainen, laajalti tuki ja helppo lukea sekä kirjoittaa ohjelmallisesti.

## How to: - Kuinka tehdään:
Käytetään `cassava`-kirjastoa CSV:n käsittelyyn Haskellissa.

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Oleta että Meillä on CSV-tiedosto nimeltään data.csv seuraavalla sisällöllä:
-- name,age
-- John Doe,30
-- Jane Smith,25

-- Määritellään tyyppi riviä varten
data Person = Person { name :: !String, age :: !Int }

instance FromNamedRecord Person where
  parseNamedRecord m = Person <$> m .: "name" <*> m .: "age"

main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, vector) -> V.forM_ vector $ \person ->
            putStrLn $ name person ++ " on " ++ show (age person) ++ " vuotias"
```

Tuloste olisi:

```
John Doe on 30 vuotias
Jane Smith on 25 vuotias
```

## Deep Dive - Syväsukellus:
CSV (Comma-Separated Values) on yksi vanhimmista tiedonvaihtoformaatteja, joka erottaa arvot pilkulla. Vaikka se on yksinkertainen, ei ole standardia, minkä takia eri järjestelmät voivat tulkita CSV-tiedostoja eri tavoin. Haskellissa `cassava` on suosittu kirjasto CSV:n kanssa työskentelyyn, mutta muitakin vaihtoehtoja, kuten `csv-conduit`, ovat olemassa. `cassava` tukee sekä nimettyjä että nimettömiä tietueita.

## See Also - Katso Myös:
- `cassava` kirjaston dokumentaatio: https://hackage.haskell.org/package/cassava
- CSV:n yleistietoa: https://tools.ietf.org/html/rfc4180
- `csv-conduit` kirjasto, toinen valinta CSV-käsittelyyn: https://hackage.haskell.org/package/csv-conduit
