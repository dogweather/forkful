---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:10.574192-07:00
description: "Gerar n\xFAmeros aleat\xF3rios em Elm envolve o uso do m\xF3dulo `Random`\
  \ para produzir n\xFAmeros pseudoaleat\xF3rios, que s\xE3o \xFAteis para uma variedade\
  \ de tarefas, como\u2026"
lastmod: '2024-03-13T22:44:46.495942-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios em Elm envolve o uso do m\xF3dulo `Random`\
  \ para produzir n\xFAmeros pseudoaleat\xF3rios, que s\xE3o \xFAteis para uma variedade\
  \ de tarefas, como\u2026"
title: "Gerando n\xFAmeros aleat\xF3rios"
weight: 12
---

## O Que & Porquê?
Gerar números aleatórios em Elm envolve o uso do módulo `Random` para produzir números pseudoaleatórios, que são úteis para uma variedade de tarefas, como jogos, simulações e até como parte de algoritmos que exigem processos estocásticos. Essa capacidade permite que os desenvolvedores adicionem imprevisibilidade e variedade às suas aplicações, melhorando a experiência do usuário e a funcionalidade.

## Como fazer:
A natureza puramente funcional de Elm significa que você não pode gerar números aleatórios diretamente como faria em linguagens imperativas. Em vez disso, você usa o módulo `Random` em conjunto com comandos. Aqui está um exemplo básico que gera um número inteiro aleatório entre 1 e 100.

Primeiro, instale o módulo `Random` com `elm install elm/random`. Em seguida, importe-o para o seu arquivo Elm, junto com os módulos HTML e de eventos necessários, assim:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Para que este seja um exemplo autocontido, você pode adicionar este código inicial:
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

Em seguida, defina um **comando** para gerar um número aleatório. Isso envolve configurar um tipo `Msg` para lidar com o número aleatório assim que for gerado, um `Model` para armazená-lo e uma função de atualização para integrar tudo.
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

Para disparar a geração de um número, você enviaria uma mensagem `Generate`, por exemplo, através de um botão na sua visualização:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Número Aleatório: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Gerar" ]
        ]
```

Quando você clica no botão "Gerar", um número aleatório entre 1 e 100 será exibido.

Essa abordagem simplista pode ser adaptada e expandida, aproveitando outras funções no módulo `Random` para produzir floats aleatórios, listas ou até estruturas de dados complexas baseadas em tipos personalizados, proporcionando um vasto campo de possibilidades para adicionar imprevisibilidade às suas aplicações Elm.

O Guia Elm entra em muito mais detalhes. Ele também tem [um exemplo de rolagem de um dado de seis lados](https://guide.elm-lang.org/effects/random).
