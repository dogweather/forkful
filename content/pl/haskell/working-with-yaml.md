---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
YAML to format danych, który jest czytelny dla ludzi i często używany do konfiguracji. Programiści pracują z YAML, aby łatwo zarządzać ustawieniami aplikacji, szczególnie w środowiskach DevOps i w projektach open source.

## Jak to zrobić:
Korzystanie z biblioteki `yaml` w Haskellu wymaga dodania jej do projektu. Przykładowo:

1. Dodawanie zależności:
   ```plaintext
   // w pliku package.yaml lub .cabal
   dependencies:
   - yaml
   ```

2. Parsowanie YAML:
   ```haskell
   {-# LANGUAGE OverloadedStrings #-}

   import Data.YAML
   import Data.Text (Text)
   import qualified Data.ByteString.Char8 as BS

   data Config = Config
     { name :: Text
     , enable :: Bool
     } deriving (Show)

   instance FromYAML Config where
     parseYAML = withObject "Config" $ \v -> Config
       <$> v .: "name"
       <*> v .: "enable"

   main :: IO ()
   main = do
     yamlData <- BS.readFile "config.yaml"
     let parsed = decode1Strict yamlData :: Either (Pos, String) Config
     print parsed
   ```

   Przykładowy plik `config.yaml`:
   ```yaml
   name: "ExampleApp"
   enable: true
   ```

   Możliwy wynik:
   ```plaintext
   Right (Config {name = "ExampleApp", enable = True})
   ```

## Deep Dive
YAML powstał w 2001 roku jako łatwiejsza do zrozumienia alternatywa dla XML i JSON. Jego nazwa znaczyła "Yet Another Markup Language", teraz "YAML Ain’t Markup Language". Alternatywami dla YAML są JSON, XML, i TOML – każdy ma swoje zalety zależną od przypadku użycia. W Haskellu, parsowanie YAML jest realizowane przez bibliotekę `yaml`, która wykorzystuje libyaml dla wydajności.

## See Also
- Specyfikacja YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Pakiet `yaml` na Hackage: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- Porównanie formatów danych: [https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats)
