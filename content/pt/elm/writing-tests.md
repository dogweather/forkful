---
title:                "Elm: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Elm?

Testes são uma parte crucial do processo de desenvolvimento de software, e o Elm não é exceção. Eles ajudam a garantir que o código esteja funcionando como esperado e a identificar possíveis bugs. Além disso, testes bem escritos aumentam a confiança dos desenvolvedores no código e facilitam a manutenção e refatoração posteriormente. Portanto, se você quer produzir um código de qualidade e robusto, escrever testes em Elm é fundamental.

## Como escrever testes em Elm

Antes de começar, certifique-se de ter instalado o [Elm Test](https://package.elm-lang.org/packages/elm-explorations/test/latest/) em seu projeto. Em seguida, crie um arquivo de teste separado para cada módulo que deseja testar, seguindo a convenção de nomenclatura `ModuleTests.elm`.

```Elm
import Main
import Test exposing (..)
import Expect exposing (..)

main : Program Never Model Msg
main =
    describe "MeuModulo" -- nome do módulo a ser testado
        [ test "Teste do meuFuncao" <|
            \_ ->
                let
                    resultado = Main.meuFuncao 5 -- chame a função que deseja testar
                in
                Expect.equal resultado 25 -- resultado esperado
        ]
```

No exemplo acima, importamos o módulo `Main` do nosso projeto e, em seguida, usamos a função `describe` para definir o nome do módulo que será testado. Em seguida, usamos a função `test` para criar um teste específico, onde chamamos nossa função `meuFuncao` com um argumento e usamos a função `Expect.equal` para verificar se o resultado é o esperado.

## Uma incursão profunda em escrever testes

Quando escrevemos testes em Elm, é importante lembrar que estamos testando o **comportamento** do código, não apenas o **resultado**. Isso significa que queremos nos certificar de que a função está sendo chamada com os argumentos corretos e que está se comportando como o esperado. Portanto, é uma boa prática ter vários testes para cada função, abrangendo diferentes cenários de entrada e saída.

Além disso, é importante manter os testes atualizados à medida que o código é alterado. Se alguma funcionalidade for adicionada, os testes devem ser atualizados para garantir que ela esteja sendo testada corretamente. E se um bug for encontrado, um teste que simule o cenário do bug deve ser adicionado, a fim de evitar que ele volte a acontecer no futuro.

Outra dica importante é usar o [QuickCheck](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-QuickCheck) para gerar dados aleatórios para testes. Isso pode ajudar a identificar possíveis problemas que não teriam sido encontrados apenas com testes manuais.

## Veja também

- [Introdução ao Elm Test](https://elmprogramming.com/intro-to-elm-test.html)
- [Documentação oficial do Elm Test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Como escrever testes em Elm - Vídeo tutorial](https://www.youtube.com/watch?v=HMV0zpZ2o8Y)