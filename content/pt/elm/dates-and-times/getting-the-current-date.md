---
date: 2024-01-20 15:14:02.712219-07:00
description: "Conseguir a data atual significa acessar o momento exato em que o c\xF3\
  digo est\xE1 sendo executado. Programadores fazem isso para marcar eventos, gerenciar\u2026"
lastmod: '2024-03-13T22:44:46.510680-06:00'
model: unknown
summary: "Conseguir a data atual significa acessar o momento exato em que o c\xF3\
  digo est\xE1 sendo executado."
title: Obtendo a data atual
weight: 29
---

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
