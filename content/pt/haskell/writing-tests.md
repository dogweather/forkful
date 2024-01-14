---
title:    "Haskell: Escrevendo testes"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que escrever testes em Haskell

Testes são extremamente importantes em qualquer linguagem de programação, e não é diferente em Haskell. Escrever testes garante que o código funciona corretamente e pode ajudar a identificar possíveis erros antes de serem encontrados em produção. Além disso, testes bem escritos podem servir como documentação para o código, tornando-o mais fácil de entender e dar manutenção. 

## Como escrever testes em Haskell

Para escrever testes em Haskell, é necessário utilizar uma biblioteca de testes como o Hspec. Primeiro, é preciso importar o módulo "Test.Hspec" e as funções de asserção necessárias:

```Haskell
import Test.Hspec
import Test.Hspec.Assertions
```

Em seguida, os testes podem ser escritos usando a função `describe`, que recebe uma descrição do que está sendo testado e uma função contendo os testes:

```Haskell
main :: IO ()
main = hspec $ do
  describe "Função de adição" $ do
    it "deve somar dois números corretamente" $ do
      sum 2 3 `shouldBe` 5
```

No exemplo acima, o primeiro parâmetro de `describe` é uma string descrevendo a função que está sendo testada, e o segundo parâmetro é uma função anônima contendo os testes, definidos usando a função `it`. Dentro de `it`, é possível usar as funções de asserção como `shouldBe` para verificar o resultado de uma operação.

## Aprofundando nos testes

Além dos testes unitários, em Haskell também é possível escrever testes de propriedades e testes de integração. Os testes de propriedades são usados para verificar se uma certa propriedade do código está sempre sendo respeitada. Por exemplo, é possível testar se a adição de dois números é sempre comutativa:

```Haskell
main :: IO ()
main = hspec $ do
  describe "Função de adição" $ do
    it "é comutativa" $ do
      property $ \x y -> sum x y `shouldBe` sum y x
```

Já os testes de integração são usados para verificar se o código está funcionando corretamente quando integrado com outras partes do sistema.

## Veja também

- [Documentação do Hspec](https://hspec.github.io/)
- [Tutorial de testes em Haskell](https://www.fpcomplete.com/blog/2017/07/quickcheck-is-cool-for-haskell/)