---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON eli JavaScript Object Notation on kevyt tiedonvaihtoformaatti. Haskell-ohjelmoijat käyttävät sitä yleensä API-kutsujen datan sekä konfiguraatioiden käsittelyyn.

## How to:
```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy as B
import Control.Monad (mzero)

-- Oletetaan, että meillä on seuraavanlainen JSON-data:
jsonInput :: ByteString
jsonInput = "{\"name\":\"Ada Lovelace\",\"age\":28}"

-- Luodaan tyyppi, joka vastaa JSON-rakennetta:
data User = User
  { name :: String
  , age  :: Int
  } deriving (Show)

-- Teemme User-tyypistä JSON-lukukelpoisen
instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "name" <*>
                         v .: "age"
  parseJSON _          = mzero

-- JSON-stringin jäsentäminen ja tulostus
main :: IO ()
main = case decode jsonInput :: Maybe User of
  Just user -> print user
  Nothing   -> putStrLn "Virhe jäsentämisessä."
```

Jos käännetään ja ajetaan, saadaan seuraava tulos:
```
User {name = "Ada Lovelace", age = 28}
```

## Deep Dive
JSON kehitettiin 2000-luvun alussa verevöittämään tehokkaampaa datanvaihtoa verkossa. Haskellissa JSONin käsittely ei ole yhtä intuitiivista kuin JavaScriptissä. Kuitenkin kirjastot kuten Aeson tekevät työstä sujuvaa. Aesonin vaihtoehtoihin kuuluu mm. `json` ja `yaml`, jotka pohjautuvat eri käyttötarpeisiin. Aeson käyttää `lazyeval`-tekniikkaa suorituskyvyn parantamiseksi, mikä mahdollistaa suurtenkin tietomäärien käsittelyn ilman muistiongelmaa.

## See Also
- Aeson GitHub-repositorio: [https://github.com/haskell/aeson](https://github.com/haskell/aeson)
- JSON-spesifikaatio: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
