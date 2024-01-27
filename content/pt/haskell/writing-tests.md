---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever testes é criar verificações automatizadas para garantir que o código faz o que esperamos. Programadores fazem isso para evitar bugs, simplificar a manutenção do código e garantir que novas funcionalidades não quebrem as existentes.

## Como Fazer:

Para escrever e rodar testes em Haskell, você pode usar a biblioteca `HSpec`. Aqui está um exemplo básico:

```Haskell
import Test.Hspec
import Data.List (sort)

-- Função simples para testar
dobraValores :: [Int] -> [Int]
dobraValores = map (*2)

main :: IO ()
main = hspec $ do
  describe "dobraValores" $ do
    it "dobra cada elemento de uma lista" $ do
      dobraValores [1,2,3,4] `shouldBe` [2,4,6,8]

  describe "sort" $ do
    it "ordena uma lista de inteiros" $ do
      sort [3,2,1,4] `shouldBe` [1,2,3,4]
```

Execute o teste com `stack test` ou `cabal test`, dependendo do seu gerenciador de pacotes. A saída esperada será algo como:

```
dobraValores
  dobra cada elemento de uma lista
sort
  ordena uma lista de inteiros
Finished in 0.0001 seconds
2 examples, 0 failures
```

## Mergulho Profundo:

Os testes em Haskell evoluíram nos últimos anos, com frameworks como `QuickCheck` para testes de propriedades e `HUnit` para testes unitários. Alternativas populares com focos diferentes incluem `doctest`, onde você pode escrever testes nas próprias docstrings do código. Ao escrever testes, priorize sempre assegurar a correção das funções e cobrir casos de uso variados, incluindo extremos e valores anômalos.

## Veja Também:

- [HSpec User Guide](http://hspec.github.io/)
- [QuickCheck Manual](https://hackage.haskell.org/package/QuickCheck)
- [Introduction to Haskell Testing with HUnit](https://wiki.haskell.org/HUnit_1.0_User's_Guide)
- [Real World Haskell, Chapter 11 - Testing and quality assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)
