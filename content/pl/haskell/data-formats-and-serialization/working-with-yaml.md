---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:37.542117-07:00
description: "Jak to zrobi\u0107: Haskell nie posiada wbudowanego wsparcia dla przetwarzania\
  \ YAML, ale mo\u017Cna u\u017Cywa\u0107 bibliotek stron trzecich, takich jak `yaml`\
  \ i `aeson`, do\u2026"
lastmod: '2024-03-13T22:44:35.473872-06:00'
model: gpt-4-0125-preview
summary: "Haskell nie posiada wbudowanego wsparcia dla przetwarzania YAML, ale mo\u017C\
  na u\u017Cywa\u0107 bibliotek stron trzecich, takich jak `yaml` i `aeson`, do parsowania\
  \ i generowania danych YAML."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
Haskell nie posiada wbudowanego wsparcia dla przetwarzania YAML, ale można używać bibliotek stron trzecich, takich jak `yaml` i `aeson`, do parsowania i generowania danych YAML. Oto, jak możesz zacząć:

### Czytanie YAML
Najpierw dodaj pakiet `yaml` do zależności swojego projektu. Następnie możesz użyć poniższego przykładu, aby przeanalizować prosty dokument YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Przykładowe dane YAML
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Zdefiniuj strukturę danych, która pasuje do dokumentu YAML
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
    Left err -> putStrLn $ "Błąd parsowania YAML: " ++ show err
    Right person -> print person
```
Przykładowe wyjście dla powyższego kodu może wyglądać tak:
```
Person {name = "John Doe", age = 30}
```

### Pisanie YAML
Aby wygenerować YAML z struktur danych Haskell, możesz użyć funkcji kodujących pakietu `yaml`, jak pokazano poniżej:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Korzystając ze struktury danych Person z poprzedniego przykładu

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
Wyjście tego programu będzie ciągiem sformatowanym w YAML:
```
name: Jane Doe
age: 25
```

Te przykłady powinny posłużyć jako punkt wyjścia do pracy z YAML w Haskellu. W zależności od Twoich potrzeb, możesz chcieć zbadać bardziej zaawansowane funkcje i opcje oferowane przez te biblioteki.
