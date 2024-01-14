---
title:                "Elm: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому
Часто наші програми повинні працювати зі структурованими даними, такими як таблиці CSV. І завдання по обробці цих даних може бути набагато простішим, якщо ми використовуємо мову програмування Elm. Elm надає нам зручні інструменти для роботи з CSV, що дозволяє легко та ефективно обробляти дані.

## Як це зробити
Щоб почати роботу з CSV у Elm, давайте спочатку імпортуємо модуль `Csv.Decode`, який надає функції для декодування та обробки CSV даних. На прикладі таблиці з даними користувачів, давайте розглянемо, як зчитати дані з CSV файлу:

```
import Csv.Decode exposing (..)

type alias User =  -- оголошуємо тип даних для користувача
    { name : String
    , age : Int
    , email : String
    }

userDecoder : Decoder User  -- створюємо декодер для типу User
userDecoder =
    map3 User  -- використовуємо функцію map для створення User з трьох рядків таблиці CSV
        (field "name" string)
        (field "age" int)
        (field "email" string)

users : Result String (List User)  -- зчитуємо дані з CSV файлу та декодуємо їх у список користувачів
users =
    decodeString (list userDecoder) "name, age, email\nJohn, 25, john@email.com\nMaria, 30, maria@email.com"
            
```

В результаті, ми отримаємо `Ok [{ name = "John", age = 25, email = "john@email.com" }, { name = "Maria", age = 30, email = "maria@email.com" }]`, що є список користувачів.

## Глибші дослідження
У Elm є багато корисних функцій для роботи з CSV, які дозволяють легко обробляти дані. Наприклад, функція `Csv.Decode.mapN` дозволяє нам об'єднувати багато декодерів у один, щоб працювати з більш складною структурою даних. Також, Elm надає можливість використовувати функції для пропуску або обробки недійсних даних, якщо вони з'являються в таблиці CSV.

## Дивіться також
- [Офіційна документація Elm для роботи з CSV](https://elm-lang.org/docs/interop/csv)
- [Пакет Csv.Encode для кодування даних у CSV формат](https://package.elm-lang.org/packages/elm-community/csv/latest/Csv-Encode)
- [Пакет Csv.Decoder для декодування даних з CSV](https://package.elm-lang.org/packages/NoRedInk/elm-csv/latest/Csv-Decode)