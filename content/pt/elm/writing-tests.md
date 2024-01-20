---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Escrever testes é criar um conjunto de procedimentos que verificam se o seu código faz o que é suposto fazer. Programadores fazem isso para garantir qualidade, prevenir erros e facilitar a manutenção do código.

## Como Fazer:
Para escrever testes em Elm, você usufruirá do `elm-test`, que é uma biblioteca padrão para esse fim. Instale com `elm install elm-explorations/test`. Depois, escreva e execute testes simples:

```Elm
import Expect
import Test exposing (..)

suiteDeTestes : Test
suiteDeTestes =
    describe "Exemplo de Testes"
        [ test "Teste de soma simples" <| 
            \_ -> Expect.equal 4 (2 + 2)
        , test "Teste de falha proposital" <| 
            \_ -> Expect.equal 5 (2 + 2)
        ]

-- Para correr os testes, execute no terminal:
-- elm-test
```

Você verá resultados no terminal apontando se os testes passaram ou falharam.

## Mergulho Profundo
O Elm foi criado com uma filosofia de facilidade e confiabilidade em mente, e isso inclui a escrita de testes. O `elm-test` fornece uma estrutura para testes unitários e de integração. Historicamente, ele evoluiu junto com a linguagem para tornar a experiência de teste tão agradável quanto o desenvolvimento com Elm. Embora o `elm-test` seja a escolha predominante, outras ferramentas como `elm-check` têm oferecido alternativas para testes de propriedades. A implementação de testes no Elm é direta e a linguagem deixa mais difícil cometer erros, o que torna a dependência em testes um pouco menos crítica que em outras linguagens, mas ainda muito importante.

## Veja Também
- [Elm Test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm Programming Language](https://elm-lang.org/)
- [RealWorld example app em Elm](https://github.com/rtfeldman/elm-spa-example) - Um projeto Elm completo com exemplos de testes.