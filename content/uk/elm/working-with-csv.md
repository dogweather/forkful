---
title:                "Робота з csv"
html_title:           "Elm: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?

Робота з CSV є досить поширеною серед програмістів. CSV це формат для зберігання та обміну даними у вигляді таблиць. Це досить легкий та зручний спосіб зберігання великих обсягів даних, тому програмісти використовують його для роботи зі збреженням та обробкою даних.

## Як використовувати:

Для роботи з CSV у Elm потрібно використовувати модуль ```elm/parser``` та функцію ```decode```. Наприклад, для декодування CSV файлу, який містить назви книжок та їх авторів, ми можемо використовувати наступний код:

```elm
import Csv exposing (..)
import File exposing (readFile)
import Parser exposing (..)

type alias Book =
    { title : String
    , author : String
    }

decodeBook : Decode.Decoder Book
decodeBook =
    Decode.map2 Book
        (field "title" string)
        (field "author" string)

readCsv : String -> Cmd msg
readCsv fileName =
    let
        decoder =
            Decode.map (List.map decodeBook)
                (decodeString (map Decode.oneOf [decodeRow, Decode.succeed []]))
    in
    readFile fileName
        |> Cmd.andThen (\result -> case result of
            Ok content ->
                Csv.parse decoder content

            Err error ->
                -- handle error
        )

decodeRow : Decode.Decoder (List String)
decodeRow =
    Decode.field "Row data" (list string)
        |> Decode.succeed
```

Результатом буде список книжок та авторів, який можна використовувати для подальшої обробки.

## Глибше вдивимося:

Формат CSV був створений у 1972 році та став дуже популярним у зв'язку зі зростанням використання простих баз даних. На відміну від більш складних форматів, таких як XML, CSV є дуже простим та зрозумілим. Альтернативами до роботи з CSV у Elm можуть бути використання різних бібліотек, таких як ```elm-csv```, ```elm-decode-csv```, або реалізація власного декодера за допомогою модуля ```elm/parser```. 

При роботі з CSV необхідно враховувати можливі помилки, такі як неправильний формат файлу або відсутність певних даних. Також важливо використовувати правильні типи даних при декодуванні, щоб уникнути непередбачуваних помилок у подальшій обробці даних.

## Дивіться також:

- Офіційна документація Elm: https://elm-lang.org
- Розширення Elm для роботи з CSV: https://package.elm-lang.org/packages/elm-explorations/csv/latest/
- Приклади роботи з CSV у Elm: https://github.com/ggb/elm-csv