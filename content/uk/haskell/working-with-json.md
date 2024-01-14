---
title:                "Haskell: Робота з json"
simple_title:         "Робота з json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

JSON (JavaScript Object Notation) - це формат обміну даними, який широко використовується у програмуванні. Він дуже популярний у розробці веб-додатків, оскільки його легко читати людям та машинам. Робота з JSON дозволяє розбирати та створювати структуровані дані, що зробить ваші програми більш функціональними та ефективними.

## Як:

Щоб почати роботу з JSON у Haskell, необхідно встановити пакет `aeson`, який надає засоби для роботи з цим форматом. Після цього вам потрібно імпортувати модуль `Data.Aeson` у ваш проект.

Простий спосіб розбору JSON-даних у Haskell - це використання функції `decode`. Наприклад, якщо ми маємо наступний JSON-об'єкт:

```Haskell
{
  "name": "John",
  "age": 30
}
```

Ми можемо розпарсити його у Haskell-значення, використовуючи наступний код:

```Haskell
import Data.Aeson

jsonString = "{\"name\":\"John\",\"age\":30}"

-- Розпарсимо наш JSON-об'єкт та виведемо значення поля "name"
main :: IO ()
main = do
    let maybePerson = decode jsonString :: Maybe Person
    case maybePerson of
        Just person -> putStrLn $ name person
        Nothing -> putStrLn "Невдалось розпарсити JSON"
    where name person = "Ім'я: " ++ (Data.Aeson.fromJSON (Data.Aeson.Object object) :: String)
                    where object = "name" Data.Aeson..= name person
```

Отримана в результаті виведе на екран `Ім'я: John`.

## Глибше

Почати роботу з JSON у Haskell набагато обширніше, ніж просте розбирання даних. Вам не тільки потрібно буде вчитися розпарсувати дані, але і створювати їх власноруч. Для цього, пакет `aeson` надає такі типи як `Object`, `Array`, `Integer` та багато інших.

Також, ви можете серіалізувати дані назад у JSON-об'єкт використовуючи функцію `encode`.

## Дивитися також

- [Офіційна документація пакету `aeson`](https://hackage.haskell.org/package/aeson)
- [Стаття "Подивимося на Aeson"](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/competition-winners/andrew-gibiansky/aeson) від автора пакету `aeson`.