---
title:                "Arbeid med YAML"
date:                  2024-01-19
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML håndtering i Haskell lar deg lese og skrive data i et klart og menneskelesbart format. Programmerere bruker YAML for konfigurasjonsfiler og datautveksling fordi det er enkelt og fleksibelt.

## How to:
For å jobbe med YAML i Haskell trenger du `yaml` pakken. Installer den med `cabal install yaml` eller `stack add yaml`. Her er et eksempel på hvordan du leser og skriver YAML:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import Control.Exception (catch)

-- Definer en enkel datastruktur
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)

instance ToJSON Person where
  toJSON (Person name age) =
    object ["name" .= name, "age" .= age]

instance FromJSON Person where
  parseJSON (Object v) =
    Person <$> v .: "name"
           <*> v .: "age"
  parseJSON _ = fail "Forventet et objekt for Person"

main :: IO ()
main = do
  -- Skriv til en YAML-fil
  BS.writeFile "person.yaml" (encode (Person "Ola Nordmann" 30))

  -- Les fra en YAML-fil
  yamlData <- BS.readFile "person.yaml"
  let maybePerson = decode yamlData :: Maybe Person
  
  case maybePerson of
    Just person -> print person
    Nothing -> putStrLn "Kunne ikke dekode YAML."
```

Kjøring av `main` vil skrive `Person` til `person.yaml`, lese filen, og utskrift blir `Person {name = "Ola Nordmann", age = 30}`.

## Deep Dive
YAML startet omkring 2001 som et mer leselig alternativ til XML. I Haskell håndterer biblioteket `yaml` parsing og generering av YAML-data. Som alternativ kan du bruke `aeson-yaml` for å jobbe direkte med JSON-til-YAML-konvertering. Implementasjonen bygger på `libyaml`, en C-bibliotek for YAML-operationer. For effektivitet parses YAML direkte til Haskell-datastrukturer.

## See Also
- YAML spesifikasjon: https://yaml.org/spec/1.2/spec.html
- `yaml` pakken på Hackage: https://hackage.haskell.org/package/yaml
- `aeson-yaml` for JSON-YAML-konverteringer: https://hackage.haskell.org/package/aeson-yaml
- Offisiell YAML hjemmeside med ekstra ressurser: https://yaml.org/
