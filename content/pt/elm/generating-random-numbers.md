---
title:                "Geração de números aleatórios"
date:                  2024-01-27T20:33:28.500354-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Gerar números aleatórios em Elm envolve criar valores numéricos imprevisíveis que são essenciais para aplicações como jogos, simulações e algoritmos de segurança. Programadores usam a aleatoriedade para simular variabilidade do mundo real, melhorar a experiência do usuário ou proteger dados com técnicas de criptografia.

## Como:
Elm lida com a aleatoriedade de maneira diferente de muitas linguagens de programação, utilizando um sistema que mantém as funções puras. Para gerar números aleatórios, você deve trabalhar com o módulo `Random` do Elm. Aqui está um exemplo básico de geração de um número aleatório entre 1 e 100:

```Elm
import Html expondo (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

Este trecho usa `Random.generate` para criar um comando que, quando executado, produz um número aleatório dentro do intervalo especificado. A declaração `type Msg` é usada para lidar com o número gerado na função de atualização da sua aplicação Elm.

Para um exemplo mais interativo, vamos olhar para um cenário onde usuários ativam a geração de número aleatório através de um clique:

```Elm
import Html expondo (Html, button, div, text)
import Html.Events expondo (onClick)
import Random

type alias Model = Int

type Msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber (Random.int 1 100))

view : Model -> Html Msg
view model =
    div []
        [ text ("Número gerado: " ++ String.fromInt model)
        , button [ onClick Generate ] [ text "Gerar novo número" ]
        ]

type Msg = NewRandomNumber Int
```

Esta aplicação Elm introduz interatividade, atualizando a exibição com um novo número aleatório cada vez que o usuário clica no botão.

## Aprofundamento
O design do sistema de geração de números aleatórios do Elm decorre do comprometimento da linguagem com a pureza e previsibilidade. Em vez de funções impuras diretas que retornam valores diferentes a cada chamada, Elm encapsula a aleatoriedade em uma estrutura `Cmd`, alinhando com sua arquitetura que separa efeitos colaterais de funções puras.

Embora essa abordagem garanta consistência no comportamento da aplicação e facilite a depuração, ela introduz uma curva de aprendizado para aqueles acostumados com a geração imperativa de números aleatórios. No entanto, os benefícios de manter a pureza da aplicação e a facilidade de teste geralmente superam a complexidade inicial.

O método do Elm também contrasta com as linguagens que oferecem geradores globais de números aleatórios, que podem levar a bugs sutis devido ao estado compartilhado. Ao exigir o tratamento explícito da geração de números aleatórios e seus efeitos, Elm incentiva os desenvolvedores a pensar mais criticamente sobre onde e como a aleatoriedade afeta suas aplicações, levando a um código mais robusto e previsível.

Para alternativas, outras linguagens funcionais oferecem funcionalidades semelhantes, mas podem implementá-las de maneira diferente. Haskell, por exemplo, também mantém a pureza na geração de números aleatórios, mas por meio do uso de monads, um conceito que o Elm evita deliberadamente para simplificar seu modelo. Comparativamente, a abordagem do Elm é mais acessível para novatos e enfatiza uma arquitetura de aplicação direta sem sacrificar o poder dos princípios de programação funcional.
