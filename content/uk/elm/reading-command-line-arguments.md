---
title:                "Читання аргументів командного рядка"
html_title:           "Elm: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Тому, чому

Все, що ми робимо на комп'ютері, починається зі зміни командного рядка. Чи то запускати програму, чи зчитувати вхідні дані, чи використовувати аргументи командного рядка, ми повинні знати, як користуватися цими можливостями для ефективної роботи. Тому, якщо ви хочете вдосконалити свої навички Elm, ця стаття про зчитування аргументів командного рядка буде корисна для вас.

## Як

```Elm
import Platform.Cmd
import Json.Decode exposing (Decoder, decodeValue, field, int, string, succeed)
import Console exposing (log)

-- Отримуємо аргументи з командного рядка
main : Program () Model Msg
main = Platform.program
       { init = init
       , update = update
       , view = view
       , subscriptions = \_ -> Sub.none
       }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        arguments = Platform.Cmd.getArgs
    in
        ( Model arguments, Cmd.none )

-- Модель аргументів командного рядка
type alias Model =
    { arguments : List String
    }

-- Наш приклад Decoder, який буде використовуватися для парсингу JSON
type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decoder Person
personDecoder =
    succeed Person
        |> field "name" string
        |> field "age" int

-- Парсимо JSON аргументи з командного рядка
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.arguments of
        argument :: _ ->
            case decodeValue personDecoder argument of
                Ok person ->
                    ( { model | arguments = List.tail model.arguments }, log person )

                Err error ->
                    ( model, log error )

        _ ->
            ( model, Cmd.none )

-- Виводимо результат в консолі
view : Model -> Html msg
view model =
    div []
        [ text "Зчитані аргументи командного рядка:"
        , ul [] (List.map (\argument -> li [] [ text argument ]) model.arguments)
        ]

```

Можливий вихід у консолі:

```
Зчитані аргументи командного рядка:
  -h
  {"name":"John", "age": 25}
```

## Глибоке занурення

Ви можете використовувати не тільки `getArgs` для отримання аргументів командного рядка, але і `getEnv` для отримання змінних середовища. Крім того, ви можете використовувати `Decoder` не тільки для парсингу JSON, але і для інших форматів даних, таких як CSV або XML. Також, ви можете використовувати команди `Cmd` для запуску команд у терміналі з вихідними даними зчитаних аргументів.

## Дивіться також

- [Документація Elm по роботі з командним рядком](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Стаття про парсинг JSON в Elm](https://elmprogramming.com/decoding-json-in-elm.html)
- [Стаття про використання команд в Elm](https://elmprogramming.com/using-commands-in-elm.html)