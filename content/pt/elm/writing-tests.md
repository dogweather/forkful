---
title:                "Elm: Escrevendo testes"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Elm?

Escrever testes é uma parte importante do desenvolvimento de qualquer programa, e em Elm isso não é diferente. Testes ajudam a garantir que o código esteja funcionando corretamente, identificando possíveis erros e bugs antes mesmo deles acontecerem. Além disso, testes ajudam a documentar o código, facilitando a compreensão para outros desenvolvedores.

## Como escrever testes em Elm

Para escrever testes em Elm, é necessário utilizar o pacote `elm-test`, que já vem incluído na linguagem. Primeiro, certifique-se de ter o pacote instalado em seu computador. Em seguida, crie um diretório `tests` em seu projeto para guardar seus testes.

A estrutura básica de um teste em Elm é a seguinte:

```elm
test "nome_do_teste" <|
    \() ->
        -- código a ser testado aqui
        Expect.equal resultado_esperado resultado_atual
```

É importante notar que os testes são funções em Elm, e por isso devem ser transcritos utilizando a sintaxe de funções. No exemplo acima, utilizamos a função `test` para definir nosso teste, passando como argumento o nome do teste e uma função anônima, que contém o código a ser testado.

Dentro da função anônima, utilizamos a função `Expect.equal` para comparar o resultado esperado com o resultado atual do código. Em caso de falha no teste, essa função irá retornar um erro indicando a diferença entre os dois valores.

Após escrever seus testes, basta rodar o comando `elm-test` no diretório `tests` para executá-los.

## Aprofundando em escrever testes

Para criar uma suíte de testes mais robusta, é possível utilizar funções auxiliares como `Expect.notEqual`, `Expect.true`, `Expect.false`, entre outras, para testar diferentes tipos de valores e condições.

Também é possível utilizar a biblioteca `elm-community/elm-test-extra` para ter acesso a funções adicionais, como testes de exceções e testes de funções que aceitam como argumento sinais de diferença de precisão para valores decimais.

Outra dica é utilizar o módulo `Test.Random`, que permite a geração de dados aleatórios para testar funções mais complexas.

## Veja também

- Documentação oficial do `elm-test`: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Documentação do `elm-community/elm-test-extra`: https://package.elm-lang.org/packages/elm-community/elm-test-extra/latest/