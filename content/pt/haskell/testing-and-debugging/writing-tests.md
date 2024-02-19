---
aliases:
- /pt/haskell/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:51.768747-07:00
description: "Escrever testes em Haskell \xE9 sobre garantir que suas fun\xE7\xF5\
  es funcionem como esperado atrav\xE9s de verifica\xE7\xF5es autom\xE1ticas. Programadores\
  \ fazem isso para\u2026"
lastmod: 2024-02-18 23:08:58.202661
model: gpt-4-0125-preview
summary: "Escrever testes em Haskell \xE9 sobre garantir que suas fun\xE7\xF5es funcionem\
  \ como esperado atrav\xE9s de verifica\xE7\xF5es autom\xE1ticas. Programadores fazem\
  \ isso para\u2026"
title: Escrevendo testes
---

{{< edit_this_page >}}

## O Que & Por Que?

Escrever testes em Haskell é sobre garantir que suas funções funcionem como esperado através de verificações automáticas. Programadores fazem isso para capturar bugs precocemente, facilitar o refatoramento e documentar o comportamento, tornando a base de código mais sustentável e escalável.

## Como Fazer:

Haskell suporta vários frameworks de teste, mas dois populares são `Hspec` e `QuickCheck`. O Hspec permite que você defina especificações legíveis por humanos para o seu código, enquanto o QuickCheck permite gerar testes automaticamente descrevendo propriedades que seu código deve satisfazer.

### Usando Hspec

Primeiro, adicione `hspec` à configuração da sua ferramenta de build (por exemplo, `stack.yaml` ou arquivo `cabal`). Em seguida, importe `Test.Hspec` e escreva testes como especificações:

```haskell
-- arquivo: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "adiciona dois números" $
    add 1 2 `shouldBe` 3

  it "retorna o primeiro número quando adiciona zero" $
    add 5 0 `shouldBe` 5
```

Então, execute seus testes usando sua ferramenta de build, resultando em uma saída que pode parecer com:

```
MyLib.add
  - adiciona dois números
  - retorna o primeiro número quando adiciona zero

Terminado em 0.0001 segundos
2 exemplos, 0 falhas
```

### Usando QuickCheck

Com QuickCheck, você expressa propriedades que suas funções devem satisfazer. Adicione `QuickCheck` à configuração do seu projeto e, em seguida, importe-o:

```haskell
-- arquivo: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Executando esses testes, entradas são geradas automaticamente para verificar as propriedades especificadas:

```
+++ OK, passou em 100 testes.
+++ OK, passou em 100 testes.
```

Nos exemplos de tanto Hspec quanto QuickCheck, as suítes de testes servem como documentação executável que pode verificar automaticamente a correção do seu código.
