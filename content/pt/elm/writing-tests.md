---
title:    "Elm: Escrevendo Testes"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Elm?

Se você já trabalha com programação há algum tempo, provavelmente já ouviu falar sobre a importância de escrever testes para garantir a qualidade do seu código. Mas por que isso é tão importante quando se trata de programação em Elm? A resposta é simples: testes aumentam a confiabilidade e a estabilidade do seu código, ajudando a prevenir erros e facilitando a manutenção do seu programa a longo prazo.

## Como escrever testes em Elm

Para escrever testes em Elm, você precisará usar o módulo `elm-test`. Este módulo possui diversas funções que facilitam a criação de testes para suas funções e componentes. Vamos ver um exemplo simples de como escrever e executar um teste em Elm:

````Elm
module Testes exposing (..)

import Test exposing (..)

soma : Int -> Int -> Int
soma x y =
    x + y

testesSoma =
    describe "soma"
        [ test "soma 1 + 2 é igual a 3" <|
            \_ -> Expect.equal (soma 1 2) 3
        ]

main =
    run testesSoma
````

No código acima, nós definimos uma função `soma` que recebe dois números como parâmetro e retorna a soma deles. Em seguida, criamos um teste utilizando a função `test` do módulo `Test`. Dentro deste teste, utilizamos a função `Expect.equal` para verificar se o resultado da função `soma` é igual a 3 quando passamos os valores 1 e 2 como argumentos. Por fim, executamos o teste utilizando a função `run` e passamos como argumento nossa função `testesSoma`.

Ao rodar este código, o módulo `elm-test` irá executar o teste e nos retornar o resultado. Se tudo estiver funcionando corretamente, veremos uma mensagem de sucesso indicando que nosso teste passou.

## Aprofundando nos testes em Elm

Além de simplesmente testar as saídas esperadas de uma função, é importante também testar possíveis entradas inválidas e verificar se o código é capaz de lidar com elas. O módulo `elm-test` possui funções como `Expect.throws` e `Expect.catch` que podem nos ajudar a fazer isso. Também é possível escrever testes para componentes mais complexos, testar efeitos colaterais e integrar os testes com ferramentas de CI/CD.

Se você quer se aprofundar ainda mais nos testes em Elm, recomendamos a leitura da documentação oficial do módulo `elm-test` e a prática constante escrevendo testes para seus próprios projetos.

## Veja também

- [Documentação oficial do módulo elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Exemplos de testes em Elm](https://jackfranklin.co.uk/blog/property-based-testing-with-elm/)
- [Guia de boas práticas para escrever testes em Elm](https://www.algonauti.com/blog/testing-in-elm-best-practices/)