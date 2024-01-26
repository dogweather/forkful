---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Що та Чому?
JSON (JavaScript Object Notation) - це формат обміну даними, легкий для людського сприйняття і машинного парсингу. Програмісти використовують його для збереження і передачі структурованої інформації між сервером і клієнтом або в рамках систем.

## Як це зробити:
В Haskell ми працюємо з JSON за допомогою пакету `aeson`. Нижче - приклади коду та результат їх роботи.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy as B
import Data.Text
import Control.Monad
import Data.Maybe

-- Десеріалізація JSON в Haskell
jsonData :: ByteString
jsonData = "{\"name\":\"Max\", \"age\":25}"

-- Тип для відповідності структури JSON
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving Show

instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
      <$> v .: "name"
      <*> v .: "age"
      
main :: IO ()
main = do
  let maybePerson = decode jsonData :: Maybe Person
  case maybePerson of
    Nothing -> putStrLn "Couldn't parse JSON"
    Just person -> print person
```

Результат:
`Person {name = "Max", age = 25}`

## Поглиблено:
JSON з'явився у 2000-х роках як альтернатива XML. Він легший і швидший при парсингу. В Haskell `aeson` є стандартом для роботи з JSON. Він використовує класи типів, як `FromJSON` і `ToJSON`, для конвертації даних між Haskell об'єктами та JSON. Альтернативи `aeson` включають `json` і `jsonb`, але `aeson` кращий у виконанні і можливостях.

## Також дивіться:
- Офіційна документація по `aeson`: http://hackage.haskell.org/package/aeson
- Вікі з прикладами Haskell/JSON: https://wiki.haskell.org/Json
- Посібник з Haskell для роботи з JSON: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
