---
title:    "Elm: Gerando números aleatórios"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Elm?

Gerar números aleatórios é uma tarefa comum em muitas áreas da programação, incluindo Elm. Isso pode ser útil em jogos, simulações, sorteios e muito mais. Neste artigo, vamos explorar como gerar números aleatórios em Elm e como usar essa funcionalidade em seus projetos.

## Como fazer isso em Elm

Para gerar números aleatórios em Elm, usamos a biblioteca `Random` incluída no núcleo da linguagem. Primeiro, importamos essa biblioteca no início do nosso arquivo Elm:

```
import Random
```

Em seguida, precisamos definir o tipo de dado que queremos gerar aleatoriamente, por exemplo, se queremos gerar um número inteiro entre 1 e 10, usamos o tipo `Int` e especificamos o intervalo usando `Random.int`:

```
randomNumber : Random.Generator Int
randomNumber =
    Random.int 1 10
```

Agora, podemos usar essa função `randomNumber` para gerar um número aleatório sempre que precisarmos. Por exemplo, podemos usá-lo para criar um jogo simples de adivinhar o número:

```
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

import Random exposing (Generator)
import Random.Extra exposing (step)

main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { randomNumber : Int
    , guess : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { randomNumber = 0
      , guess = ""
      }
    , step randomNumber
    )

type Msg
    = NewRandomNumber Int
    | UpdateGuess String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomNumber number ->
            ( { model | randomNumber = number }
            , Cmd.none
            )

        UpdateGuess newGuess ->
            ( { model | guess = newGuess }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Random.generate NewRandomNumber randomNumber

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Tente adivinhar o número entre 1 e 10" ]
        , div [ style "font-size" "40px" ]
            [ text model.guess ]
        ]
```

Neste exemplo, usamos a função `step` do módulo `Random.Extra` para gerar um número aleatório sempre que o usuário acerta a resposta.

## Aprofundando no conceito de gerar números aleatórios em Elm

Em Elm, é importante entender que a função `randomNumber` que criamos não retorna de fato um número aleatório, mas sim uma descrição de como gerar um número aleatório. Isso significa que podemos usar essa descrição várias vezes sem precisar gerar um novo número aleatório toda vez.

Além disso, se quisermos gerar um número aleatório com um tipo de dado personalizado, podemos criar uma função específica para isso e combiná-la com as funções disponíveis na biblioteca `Random`, como `map` e `andThen`.

## Veja também

- [Documentação da biblioteca `Random`](https://package.elm-lang.org/packages/elm/random/latest/)
- [Artigo sobre a geração de números aleatórios em Elm](https://medium.com/@brianthicks/how-to-generate-random-values-in-elm-c3b053c812f7)
- [Artigo sobre a biblioteca `Random` no Elm Weekly](https://elmweekly.nl/random-and-pure-generative-testing-f8bbee024ee3)