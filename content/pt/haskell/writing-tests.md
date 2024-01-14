---
title:    "Haskell: Escrevendo testes"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante em Haskell?

Escrever testes é essencial para garantir a qualidade e confiabilidade do código em qualquer linguagem de programação, incluindo Haskell. Além disso, ao escrever testes, você pode identificar e corrigir erros em seu código de forma eficiente, economizando tempo e esforço no longo prazo.

## Como escrever testes em Haskell

Aqui está um exemplo prático de como escrever e executar testes em Haskell usando a biblioteca Hspec:

```
-- Importando as bibliotecas necessárias
import Test.Hspec
import Data.List

-- Definindo uma função simples que retorna o número de elementos em uma lista
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- Escrevendo testes usando a sintaxe "describe" e "it" do Hspec
main :: IO ()
main = hspec $ do
  describe "length" $ do
    it "retorna 0 para uma lista vazia" $ do
      length [] `shouldBe` 0
    it "retorna o número correto de elementos em uma lista" $ do
      length [1, 2, 3] `shouldBe` 3
```

Ao executar o teste acima, o resultado será:

```
length
  - retorna 0 para uma lista vazia
  - retorna o número correto de elementos em uma lista

Finished in 0.0001 seconds
2 examples, 0 failures
```

## Aprofundando em como escrever bons testes

Além da sintaxe básica apresentada acima, existem outras técnicas úteis para escrever testes eficazes em Haskell. Aqui estão algumas dicas:

- Considere testar diferentes casos de borda para garantir que seu código lida adequadamente com eles.
- Use a função `shouldBe` para comparar resultados em vez de `==`, pois `shouldBe` fornece informações mais precisas em caso de falha.
- Considere o uso de geradores de dados para testar seu código com entradas maiores ou aleatórias.
- Procure por bibliotecas de testes específicas para as funcionalidades que você está testando, como QuickCheck para testes de propriedade.

## Veja também

- [Documentação oficial do Hspec](https://hspec.github.io/)
- [Artigo sobre testes com QuickCheck em Haskell](https://medium.com/ouroboros/testes-com-quickcheck-em-haskell-f8485ab89bc2)
- [Livro "Learn You a Haskell for Great Good!" com capítulo sobre testes](http://learnyouahaskell.com/for-great-good)