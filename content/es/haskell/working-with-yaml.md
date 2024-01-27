---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
YAML es un formato para escribir datos de forma legible, usado comúnmente para archivos de configuración. Los programadores lo usan por su simplicidad y legibilidad, facilitando el manejo de datos sin las complicaciones de otros formatos como XML.

## Cómo hacerlo:
Para trabajar con YAML en Haskell, usamos la librería `yaml`. Primero, instálala con Cabal o Stack:

```bash
cabal install yaml
# o
stack install yaml
```

Ahora, para leer y escribir datos YAML, puedes usar el siguiente código Haskell:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Define un tipo de datos para coincidir con la estructura YAML.
data Config = Config {
    settingA :: String,
    settingB :: Int
    } deriving Show

-- Instancia para que nuestro tipo de datos pueda ser parseado desde YAML.
instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .: "settingA"
               <*> v .: "settingB"
    parseJSON _ = fail "Expected an Object for Config"

-- Lee el archivo YAML y devuelve el resultado.
main :: IO ()
main = do
    yamlData <- BS.readFile "config.yaml"
    let parsedConfig = decodeEither' yamlData :: Either ParseException Config
    case parsedConfig of
      Left problem -> putStrLn $ "Error parsing YAML: " ++ show problem
      Right config -> print config
```

Suponiendo que `config.yaml` es:

```yaml
settingA: 'Hello'
settingB: 123
```

La salida será `Config {settingA = "Hello", settingB = 123}`.

## Exploración Profunda
YAML, que significa "YAML Ain't Markup Language", fue introducido en 2001. Es un superconjunto de JSON, lo que significa que cualquier JSON es válido en YAML. Alternativas incluyen JSON y XML, pero YAML es preferido cuando la legibilidad humana es importante. La librería `yaml` en Haskell procesa YAML usando un enfoque nativo y proporciona funciones para convertir datos entre YAML y estructuras de datos de Haskell.

## Ver También
Para más información, mira estos recursos:

- Documentación oficial de la librería `yaml`: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- Especificación YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Tutorial de Haskell: [https://www.haskell.org/tutorial/](https://www.haskell.org/tutorial/)
- Para una comparación entre JSON, XML y YAML: [https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats)
