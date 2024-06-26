---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:49.378371-07:00
description: "Jak to zrobi\u0107: Czysto funkcyjna natura Elm oznacza, \u017Ce nie\
  \ mo\u017Cesz bezpo\u015Brednio generowa\u0107 liczb losowych, jak mog\u0142oby\
  \ to by\u0107 w j\u0119zykach imperatywnych.\u2026"
lastmod: '2024-03-13T22:44:35.317191-06:00'
model: gpt-4-0125-preview
summary: "Czysto funkcyjna natura Elm oznacza, \u017Ce nie mo\u017Cesz bezpo\u015B\
  rednio generowa\u0107 liczb losowych, jak mog\u0142oby to by\u0107 w j\u0119zykach\
  \ imperatywnych."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
Czysto funkcyjna natura Elm oznacza, że nie możesz bezpośrednio generować liczb losowych, jak mogłoby to być w językach imperatywnych. Zamiast tego używasz modułu `Random` w połączeniu z komendami. Oto podstawowy przykład, który generuje losową liczbę całkowitą między 1 a 100.

Najpierw zainstaluj moduł `Random` za pomocą `elm install elm/random`. Następnie zaimportuj go do pliku Elm wraz z potrzebnymi modułami HTML i zdarzeń, tak jak poniżej:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Aby to był samowystarczalny przykład, możesz dodać ten kod startowy:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Następnie zdefiniuj **komendę** do generowania losowej liczby. Obejmuje to ustawienie typu `Msg` do obsługi wygenerowanej liczby losowej, `Model` do jej przechowywania oraz funkcję aktualizującą, aby wszystko połączyć.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Aby zainicjować generowanie numeru, możesz wysłać wiadomość `Generate`, na przykład, poprzez przycisk w swoim widoku:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

Kiedy klikniesz przycisk "Generate", zostanie wyświetlona losowa liczba między 1 a 100.

To uproszczone podejście można dostosować i rozszerzyć, wykorzystując inne funkcje w module `Random` do produkcji losowych liczb zmiennoprzecinkowych, list lub nawet złożonych struktur danych opartych na typach niestandardowych, zapewniając ogromne możliwości dodawania nieprzewidywalności do aplikacji Elm.

Przewodnik Elm zawiera znacznie więcej informacji. Znajduje się tam również [przykład rzutu sześcienną kostką](https://guide.elm-lang.org/effects/random).
