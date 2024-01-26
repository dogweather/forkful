---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
JSON, c'est de l'info formatée clair pour machines et humains. Les développeurs l'utilisent pour échanger des données entre serveurs et apps, prônant simplicité, légèreté et accessibilité.

## Comment faire :
Haskell propose `aeson` pour jouer avec JSON. On installe avec `cabal install aeson`. Voilà comment on s'y prend:

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

-- On définit un type de données custom
data Utilisateur = Utilisateur
  { nom :: String
  , age :: Int
  } deriving (Generic, Show)

-- Ça permet d'encoder/décoder facilement.
instance ToJSON Utilisateur
instance FromJSON Utilisateur

-- Convertir depuis et vers JSON:
main :: IO ()
main = do
  let jean = Utilisateur "Jean" 30
  let jsonJean = encode jean
  B.putStrLn jsonJean -- Affiche: {"nom":"Jean","age":30}

  let decodedJean = decode jsonJean :: Maybe Utilisateur
  print decodedJean -- Affiche: Just (Utilisateur {nom = "Jean", age = 30})
```

## Exploration Approfondie
`aeson` est inspiré par JSON.hs (un ancien package). Modernisé, `aeson` est plus performant. Si `aeson` ne convient pas, il y a `json` ou `yaml`, ce dernier gère aussi YAML, proche cousin de JSON. `aeson` use de `attoparsec` pour l'analyse syntaxique, gérant les données massives efficacement.

## Voir Aussi
Enrichissez vos connaissances:
- [aeson sur Hackage](https://hackage.haskell.org/package/aeson)
- [Le parsing JSON avec aeson](https://artyom.me/aeson)
- [Un guide pour attoparsec, utilisé par aeson](https://www.stackage.org/lts/package/attoparsec)
