---
title:                "Escrevendo testes"
html_title:           "Elm: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-tests.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Escrever testes de código é uma prática comum entre programadores para garantir a qualidade e a funcionalidade do código que estão desenvolvendo. Ao escrever testes, os programadores podem identificar e corrigir erros antes da implementação do código em produção.

## Como fazer:
Elm possui uma biblioteca de testes integrada chamada `elm-test`, que permite aos programadores escrever testes para suas funções e módulos. Um exemplo simples de teste pode ser visto abaixo:

```Elm
import Test exposing (..)

add x y =
    x + y
    
testAdd =
    describe "add"
        [ test "adds two numbers" <|
            \_ ->
                add 2 3 == 5
        ]

```

A saída do teste seria algo como `Test Passed`.

## Mergulho Profundo:
A prática de escrever testes de código é frequentemente associada com o desenvolvimento ágil de software, que enfatiza a entrega de código funcional em partes menores e mais frequentes. Outras alternativas para escrita de testes incluem Cucumber e RSpec.

O `elm-test` utiliza a biblioteca `elm-expect`, que fornece funções para comparar valores, bem como `elm-json` para fazer o parser de JSON em testes.

## Veja também:
- Documentação oficial do `elm-test`: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Exemplos de uso do `elm-test`: https://github.com/elm-community/elm-test#examples
- Documentação oficial do `elm-expect`: https://package.elm-lang.org/packages/elm-explorations/elm-expect/latest/
- Documentação oficial do `elm-json`: https://package.elm-lang.org/packages/elm/json/latest/