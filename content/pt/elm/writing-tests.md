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

## Por que escrever testes em Elm?

Escrever testes é uma prática essencial para qualquer desenvolvedor. Ao escrever testes em Elm, você garante que seu código se comporte da maneira desejada e evita possíveis bugs e erros. Além disso, a escrita de testes pode economizar tempo e garantir a qualidade do seu código no longo prazo.

## Como escrever testes em Elm

Para escrever testes em Elm, é necessário utilizar a biblioteca embutida chamada `elm-test`. Com ele, você pode criar uma estrutura de testes para cada módulo em seu programa. Veja um exemplo simples de teste em Elm:

```Elm
module Main exposing (..)

import Test exposing (..)
import Expect

add : Int -> Int -> Int
add x y =
    x + y

tests : Test
tests =
    describe "Add function"
        [ test "Basic addition" <|
            \() ->
                Expect.equal (add 2 3) 5
        ]

main =
    run tests
```

Neste exemplo, importamos o módulo `Test` e a função `Expect` para comparar os resultados dos testes. Em seguida, definimos uma função `add` que será testada e criamos uma estrutura de testes com o nome "Add function". Dentro desta estrutura, temos um teste que verifica se a função `add` retorna o resultado esperado. Por fim, executamos os testes com a função `run` e obtemos o seguinte resultado:

```
Add function
    ✓ Basic addition


1 test passed in 0ms
```

## Aprofundando nos testes em Elm

Além da função `describe` que utilizamos no exemplo anterior, o `elm-test` possui outras funções para estruturar seus testes, como `suite` e `testOnly`. Além disso, você pode utilizar a função `fuzz` para gerar dados aleatórios e testar seu código com diferentes inputs.

É importante também lembrar de seguir boas práticas ao escrever testes. Dê nomes descritivos aos seus testes e evite repetição de código. Ao testar funções que possuem efeitos colaterais, utilize a função `attempts` para garantir que o teste será executado várias vezes.

## Veja Também

- Documentação oficial do `elm-test`: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Tutorial de testes em Elm: https://codemilltech.com/getting-started-testing-elm/
- Artigo sobre boas práticas em testes em Elm: https://thoughtbot.com/blog/how-we-test-elm