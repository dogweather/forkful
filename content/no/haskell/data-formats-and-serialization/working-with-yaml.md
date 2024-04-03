---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:49.349049-07:00
description: "Hvordan: Haskell har ikke innebygd st\xF8tte for prosessering av YAML,\
  \ men du kan bruke tredjepartsbiblioteker som `yaml` og `aeson` for \xE5 analysere\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.863695-06:00'
model: gpt-4-0125-preview
summary: "Haskell har ikke innebygd st\xF8tte for prosessering av YAML, men du kan\
  \ bruke tredjepartsbiblioteker som `yaml` og `aeson` for \xE5 analysere og generere\
  \ YAML-data."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Haskell har ikke innebygd støtte for prosessering av YAML, men du kan bruke tredjepartsbiblioteker som `yaml` og `aeson` for å analysere og generere YAML-data. Her er hvordan du kan komme i gang:

### Lese YAML
Først, legg til `yaml`-pakken i prosjektets avhengigheter. Deretter kan du bruke følgende eksempel for å analysere et enkelt YAML-dokument:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Eksempel på YAML-data
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Definer en datastruktur som passer med YAML-dokumentet
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Feil ved parsing av YAML: " ++ show err
    Right person -> print person
```
Eksempelutdata for ovennevnte kode kan se slik ut:
```
Person {name = "John Doe", age = 30}
```

### Skrive YAML
For å generere YAML fra Haskell-datastrukturer, kan du bruke `yaml`-pakkens kodingsfunksjonaliteter som vist nedenfor:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Bruker Person-datastrukturen fra det tidligere eksemplet

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
Utgangen av dette programmet vil være en YAML-formatert streng:
```
name: Jane Doe
age: 25
```

Disse eksemplene bør tjene som et utgangspunkt for arbeid med YAML i Haskell. Avhengig av dine behov, kan det hende du ønsker å utforske mer avanserte funksjoner og alternativer som tilbys av disse bibliotekene.
