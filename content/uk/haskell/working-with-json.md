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

## Чому

Зараз робота з JSON є необхідною для багатьох проектів, оскільки цей формат даних дозволяє зручно взаємодіяти з різноманітними джерелами та ресурсами в мережі. Також, Haskell має потужні інструменти для роботи з JSON, які значно спрощують цей процес.

## Як

Для початку, необхідно імпортувати модуль "Data.Aeson" у свій проект. Далі, щоб отримати дані з JSON файлу, скористаємося функцією "decodeFileStrict" яка приймає шлях до файлу та повертає тип "Maybe Value". За допомогою функції "fromJust" можна отримати значення типу "Value" з "Maybe Value", якщо значення не Nothing.

Пример:

```Haskell
import Data.Aeson

data Person = Person {
    name :: String,
    age :: Int,
    occupation :: String
} deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

main :: IO()
main = do
    json <- decodeFileStrict "./person.json" :: Maybe Person
    let person = fromJust json
    print person
```

В даному прикладі, із файлу "person.json" було прочитано дані та виведено об'єкт типу Person на екран.

## Глибше

Щоб зрозуміти, як працює робота з JSON у Haskell, слід розібратися з базовими структурами даних, які використовуються для збереження та обробки JSON даних. Основними типами є "Value" та "Pair", для представлення об'єктів та поля JSON відповідно.

Також, для зручної роботи зі структурами данних, можна скористатися синтаксичними розширеннями, такими як "DeriveGeneric" та "OverloadedStrings". Вони дозволяють автоматично створювати інстанси типів, а також дозволяють використати строкові літерали для представлення JSON даних.

Для детальнішого вивчення роботи з JSON у Haskell, рекомендуємо ознайомитися з документацією модуля "Data.Aeson" та подивитися додаткові приклади в Інтернеті.

## Дивіться також

- [Офіційна документація з роботи з JSON у Haskell](https://hackage.haskell.org/package/aeson)
- [Стаття "Parsing JSON in Haskell with Aeson" на Medium](https://medium.com/@jonathangfischoff/parsing-json-in-haskell-with-aeson-6adea64aeb93)
- [Стаття "Working with JSON in Haskell" на сайті "School of Haskell"](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json)