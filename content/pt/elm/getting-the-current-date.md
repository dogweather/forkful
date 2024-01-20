---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:14:02.712219-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Conseguir a data atual significa acessar o momento exato em que o código está sendo executado. Programadores fazem isso para marcar eventos, gerenciar prazos ou qualquer lógica temporal no app.

## Como fazer:

Elm torna um pouco mais envolvente pegar a data, já que funciona com imutabilidade e efeitos gerenciados. Primeiro, vamos pedir a data e depois reagir quando a conseguirmos.

```Elm
import Browser
import Html exposing (..)
import Task
import Time exposing (Posix)

type Msg = GotTime Posix

main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing, Task.perform GotTime Time.now )

type alias Model =
    { currentTime : Maybe Posix }

view : Model -> Html Msg
view model =
    div []
        [ text (case model.currentTime of
                  Just time -> String.fromInt (Time.posixToMillis time)
                  Nothing -> "Carregando..."
              )
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime time ->
            ( { model | currentTime = Just time }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
```

Saída de exemplo (formato timestamp Unix): "1619372789534".

## Aprofundamento

Elm preza pelo controle de efeitos colaterais, por isso, obter a data atual é um efeito e deve ser tratado como uma tarefa. Historicamente, Elm sempre buscou ter um modelo simples e previsível de programação, diferentemente de JavaScript que pode ser mais direto mas imprevisível em comportamento. Alternativas incluem usar flags ou portas para passar a hora do servidor para Elm, mas Time.now é a maneira canônica standard.

## Veja também

- [Elm Time documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Architecture Tutorial](https://guide.elm-lang.org/architecture/)
- [Elm Lang Discuss](https://discourse.elm-lang.org/)