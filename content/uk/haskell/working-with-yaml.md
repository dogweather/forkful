---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
YAML – це формат серіалізації даних, зручний для читання людиною. Програмісти використовують його для налаштувань, обміну даними та конфігурації проектів.

## Як це зробити:
В Haskell для роботи з YAML можна використовувати бібліотеку `yaml`. Давайте перетворимо дані з/в YAML.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml
import qualified Data.ByteString.Char8 as BS

-- Представлення наших даних в Haskell
data Config = Config
  { name :: String
  , enableLogging :: Bool
  , port :: Int
  } deriving (Show, Eq)

-- Інстанси для автоматичного (де)серіалізування
instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "name"
           <*> v .: "enableLogging"
           <*> v .: "port"

instance ToJSON Config where
  toJSON (Config name enableLogging port) =
    object ["name" .= name, "enableLogging" .= enableLogging, "port" .= port]

-- Зчитування та запис в YAML
main :: IO ()
main = do
  -- Зчитування YAML з файлу
  configData <- BS.readFile "config.yaml"
  let decodedConfig = decodeEither' configData :: Either ParseException Config
  print decodedConfig
  
  -- Запис об'єкта в YAML
  let config = Config "Server" True 8080
  BS.writeFile "output.yaml" (encode config)

```

Даний код зчитує `config.yaml`, перетворює його на `Config` і друкує. Також він серіалізує `Config` і зберігає в `output.yaml`.

## Поглиблене вивчення:
YAML виник у 2001 році як спрощення XML. В Haskell існують альтернативи: `aeson` для JSON, `tomland` для TOML. Робота з YAML у Haskell ґрунтується на швидкодії парсерів та міцній типізації.

## Дивіться також:
- [Official YAML website](https://yaml.org)
- [Hackage `yaml` package](https://hackage.haskell.org/package/yaml)
- [YAML Wikipedia article](https://uk.wikipedia.org/wiki/YAML)
