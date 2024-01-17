---
title:                "Робота з json"
html_title:           "Haskell: Робота з json"
simple_title:         "Робота з json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з JSON - це обробка структурованих даних у вигляді текстових форматів на основі JavaScript. Програмісти використовують цей формат для обміну даними між різними програмами та системами, оскільки він є зручним і легко зрозумілим для людей та комп'ютерів.

## Як?
Нижче наведені приклади коду та виведення даних для демонстрації роботи з JSON в мові Haskell:

```Haskell
import Data.Aeson

-- Десеріалізація JSON даних у тип Person
data Person = Person
  { name :: String
  , age :: Int
  , occupation :: String
  } deriving (Show)

instance FromJSON Person where
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age" <*> v .: "occupation"
    -- Вибірка значень з об'єкта JSON за допомогою оператора (<$>)

-- Приклад JSON даних
json :: ByteString
json = "{\"name\":\"John\",\"age\":25,\"occupation\":\"Developer\"}"

-- Десеріалізація та виведення даних
main :: IO ()
main = do
    let maybePerson = decode json :: Maybe Person -- Десеріалізація даних у тип Maybe
    case maybePerson of
        Just person -> print person -- Виведення даних про персону
        Nothing -> putStrLn "Invalid JSON data" -- Виведення повідомлення про помилку
```

Виведення:

```
Person {name = "John", age = 25, occupation = "Developer"}
```

## Глибше
JSON був створений Дугласом Крокфордом в 2001 році та став широко використовуваним форматом обміну даними. У мові Haskell, для роботи з JSON існують багато бібліотек, таких як "aeson" та "json". Іншими альтернативами є формати XML та CSV. Для оптимальної роботи з JSON у Haskell, варто детально ознайомитися з функціями бібліотек та вивчити особливості десеріалізації та серіалізації даних.

## Дивіться також
- [Офіційна документація по бібліотеці "aeson"](https://hackage.haskell.org/package/aeson)
- [Офіційна документація по бібліотеці "json"](http://hackage.haskell.org/package/json)