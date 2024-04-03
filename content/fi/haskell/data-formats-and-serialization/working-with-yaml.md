---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:39.704764-07:00
description: "Kuinka: Haskell ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua tukea YAML-k\xE4\
  sittelylle, mutta voit k\xE4ytt\xE4\xE4 kolmannen osapuolen kirjastoja, kuten `yaml`\
  \ ja `aeson`, YAML-\u2026"
lastmod: '2024-03-13T22:44:56.634725-06:00'
model: gpt-4-0125-preview
summary: "Haskell ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua tukea YAML-k\xE4sittelylle,\
  \ mutta voit k\xE4ytt\xE4\xE4 kolmannen osapuolen kirjastoja, kuten `yaml` ja `aeson`,\
  \ YAML-datan j\xE4sent\xE4miseen ja tuottamiseen."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
Haskell ei sisällä sisäänrakennettua tukea YAML-käsittelylle, mutta voit käyttää kolmannen osapuolen kirjastoja, kuten `yaml` ja `aeson`, YAML-datan jäsentämiseen ja tuottamiseen. Tässä on, miten voit aloittaa:

### YAMLin lukeminen
Lisää ensin `yaml`-paketti projektisi riippuvuuksiin. Tämän jälkeen voit käyttää seuraavaa esimerkkiä yksinkertaisen YAML-dokumentin jäsentämiseen:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Esimerkki YAML-data
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Määritä datarakenne, joka vastaa YAML-dokumenttia
data Henkilö = Henkilö
  { nimi :: String
  , ikä :: Int
  } deriving (Show)

instance FromYAML Henkilö where
  parseYAML = withMap "Henkilö" $ \m -> Henkilö
    <$> m .: "nimi"
    <*> m .: "ikä"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Henkilö
  case parsed of
    Left err -> putStrLn $ "Virhe YAMLin jäsentämisessä: " ++ show err
    Right henkilö -> print henkilö
```
Edellä mainitun koodin tuloste voisi näyttää seuraavalta:
```
Henkilö {nimi = "John Doe", ikä = 30}
```

### YAMLin kirjoittaminen
Haskellin datarakenteista YAMLin tuottamiseen voit käyttää `yaml`-paketin koodaustoiminnallisuuksia, kuten alla on esitetty:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Käyttäen Henkilö-datarakennetta edellisestä esimerkistä

henkilö :: Henkilö
henkilö = Henkilö "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 henkilö
  putStrLn $ unpack yamlData
```
Tämän ohjelman tuloste on YAML-muotoiltu merkkijono:
```
nimi: Jane Doe
ikä: 25
```

Nämä esimerkit toimivat lähtökohtana YAMLin käsittelylle Haskellissa. Tarpeidesi mukaan saattaisit haluta tutkia näiden kirjastojen tarjoamia monimutkaisempia ominaisuuksia ja vaihtoehtoja.
