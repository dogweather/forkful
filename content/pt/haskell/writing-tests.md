---
title:                "Escrevendo testes"
html_title:           "Haskell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Haskell?

Escrever testes é uma prática essencial para garantir a qualidade do código em qualquer linguagem de programação, e com Haskell não é diferente. Testes podem ajudar a identificar e corrigir bugs de forma mais eficiente, além de proporcionar uma maior confiança no funcionamento do código.

## Como escrever testes em Haskell

Para escrever testes em Haskell, primeiramente é necessário importar o módulo "Test.HUnit" no início do arquivo, e definir as funções de teste utilizando a sintaxe "testNomeDaFuncao = TestCase (assertEqual \"Descricao\" esperado resultado)".

```Haskell
import Test.HUnit

soma :: Int -> Int -> Int
soma x y = x + y

testSoma = TestCase (assertEqual "Deve somar dois números corretamente" 6 (soma 2 4))

tests = TestList [TestLabel "soma" testSoma]

main = runTestTT tests
```

O código acima define uma função "soma" que recebe dois números inteiros e retorna a soma entre eles. Em seguida, é criado um teste "testSoma" que verifica se a função está retornando o resultado esperado, e por fim, o teste é adicionado à lista "tests" que será executada na função "runTestTT" na função "main".

## Aprofundando-se em testes

Existem diversas ferramentas e bibliotecas disponíveis para escrever testes em Haskell, como o "HUnit" utilizado no exemplo acima, o "QuickCheck" que permite gerar casos de teste aleatoriamente, e o "Tasty" que oferece uma sintaxe mais amigável.

É importante também entender os diferentes tipos de testes, como testes unitários que verificam o comportamento de funções individuais, testes de integração que avaliam a interação entre diferentes componentes do código, e testes de propriedade que garantem que uma determinada propriedade é satisfeita pelo código em todas as situações.

## Veja também

- Documentação oficial do HUnit: https://hackage.haskell.org/package/HUnit
- Tutorial de testes em Haskell: https://wiki.haskell.org/writing_a_haskell_program_with_tests
- Biblioteca QuickCheck: https://hackage.haskell.org/package/QuickCheck
- Biblioteca Tasty: https://hackage.haskell.org/package/tasty