---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Elm: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Odczytywanie argumentów wiersza poleceń to proces w programowaniu, który polega na tym, że program przyjmuje informacje przekazane wraz z jego uruchomieniem za pomocą wiersza poleceń. Programiści używają tej techniki, ponieważ pozwala ona na przekazywanie wartości lub opcji do programu, aby modyfikować jego działanie w zależności od potrzeb.

## Jak to zrobić:
Korzystając z języka programowania Elm, odczytywanie argumentów wiersza poleceń jest proste. Można to zrobić wykorzystując funkcję `Platform.worker` oraz moduł `Platform.Cmd`.

```Elm
import Platform
import Platform.Cmd exposing (..)

main =
    Platform.worker
        { init = init
        , update = update
        , view = view
        , subscriptions = \model ->
            Sub.batch
                [ onUrlChange UrlChange
                , onBeginEnter Cmd.onBeginEnter
                ]
        }

init =
    let
        config =
            Platform.ArgParser.init Config
    in
    ( config, Cmd.none )

type Msg
    = UrlChange Url String
    | EnterBegin

type alias Config =
    { url : String
    , shouldBegin : Bool
    }

update msg model =
    case msg of
        UrlChange url ->
            { model | url = url }, Cmd.none

        EnterBegin ->
            { model | shouldBegin = True }, Cmd.none

view model =
    Html.text "Odczytanie url i opcji uruchomienia"

```

Przykładowe wyjście po odczytaniu argumentów wiersza poleceń może wyglądać następująco:

```Elm
Config
    { url = "www.example.com"
    , shouldBegin = True
    }
```

## Głębsze przyjrzenie się:
Odczytywanie argumentów wiersza poleceń jest wykorzystywane przez programistów od dawna, od czasów konsolowych programów w stylu DOS-a. Alternatywą dla tej metody jest korzystanie z plików konfiguracyjnych lub interaktywnego dialogu z użytkownikiem. W przypadku języka Elm, odczytywanie argumentów wiersza poleceń jest możliwe dzięki modułowi `Platform.Cmd`, który dostarcza funkcje do obsługi poleceń.

## Zobacz również:
* Oficjalna dokumentacja języka Elm: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
* Kurs programowania w języku Elm: [https://egghead.io/learn/elm](https://egghead.io/learn/elm)
* Forum społeczności programistów Elm: [https://elmlang.slack.com/](https://elmlang.slack.com/)