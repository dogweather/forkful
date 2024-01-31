---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con YAML in Haskell significa manipolare dati in un formato che punta a essere facile da leggere per gli umani. Lo si fa per configurazioni, serializzare dati e per lavorare con tecnologie come Kubernetes o Docker che lo adottano ampiamente.

## How to:
Per lavorare con YAML in Haskell, usiamo il pacchetto `yaml`, ottenibile con `cabal install yaml`. Ecco un esempio per leggere un file YAML e convertirlo in una struttura dati Haskell:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml
import Control.Applicative
import Data.HashMap.Strict (HashMap)
import qualified Data.ByteString.Char8 as BS

data Config = Config
  { setting1 :: String
  , setting2 :: Int
  } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "setting1"
           <*> v .: "setting2"
  parseJSON _ = empty

main :: IO ()
main = do
  file <- BS.readFile "config.yaml"
  let decoded = decodeEither' file
  case decoded of
    Left err -> print err
    Right config -> print (config :: Config)
```
Esempio di output:
```
Config {setting1 = "test", setting2 = 42}
```

Per scrivere una struttura dati in un file YAML:

```Haskell
import Data.Yaml

data User = User
  { userId :: Int
  , userName :: String
  } deriving (Show)

instance ToJSON User where
  toJSON (User id name) =
    object [ "userId" .= id
           , "userName" .= name ]

main :: IO ()
main = BS.writeFile "user.yaml" (encode (User 1 "Alessandro"))
```

Questo crea un file `user.yaml` con il seguente contenuto:
```
userId: 1
userName: Alessandro
```

## Deep Dive
YAML, che sta per "YAML Ain't Markup Language", è stato creato nel 2001. È un super-set di JSON, offrendo più flessibilità e leggibilità. Alternativamente, in Haskell si possono usare `Data.Aeson` per JSON e `xml-conduit` per XML se YAML non è richiesto. La particolarità di YAML in Haskell si ottiene attraverso l'uso di typeclass `FromJSON` e `ToJSON` per la serializzazione personalizzata, garantendo che i tipi di dati complessi siano gestiti correttamente.

## See Also
- Documentazione ufficiale di YAML: [https://yaml.org](https://yaml.org)
- Repository `yaml` Hackage: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- Introduzione a Aeson per JSON in Haskell: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
