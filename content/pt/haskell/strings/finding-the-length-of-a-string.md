---
date: 2024-01-20 17:47:44.617780-07:00
description: "Calcular o comprimento de uma string \xE9 simplesmente descobrir quantos\
  \ caracteres ela possui. Programadores fazem isso para validar entradas, limitar\
  \ texto\u2026"
lastmod: '2024-03-13T22:44:46.616589-06:00'
model: gpt-4-1106-preview
summary: "Calcular o comprimento de uma string \xE9 simplesmente descobrir quantos\
  \ caracteres ela possui."
title: Descobrindo o comprimento de uma string
weight: 7
---

## What & Why?
Calcular o comprimento de uma string é simplesmente descobrir quantos caracteres ela possui. Programadores fazem isso para validar entradas, limitar texto numa interface gráfica, ou simplesmente para manipulação de dados.

## How to:
O Haskell possui uma função integrada chamada `length` que retorna o comprimento de uma lista, incluindo strings que, em Haskell, são listas de caracteres. Veja só:

```haskell
main :: IO ()
main = do
    let minhaString = "Olá, Haskell!"
    print $ length minhaString
```

Saída esperada:

```
13
```

## Deep Dive
Historicamente, em Haskell, a função `length` é parte do Prelude, um módulo importado por padrão. A `length` funciona contando cada elemento em uma estrutura de dados até que todos sejam contabilizados.

Existem alternativas. Por exemplo, você pode usar recursão para criar sua própria função de comprimento:

```haskell
comprimento :: [a] -> Int
comprimento [] = 0
comprimento (_:xs) = 1 + comprimento xs
```

Há também um pacote chamado `Data.Text` para trabalhar com texto Unicode de maneira mais eficiente do que com Strings clássicas. Em `Data.Text`, você usaria a função `length` do mesmo jeito, mas ela seria mais performática.

```haskell
import qualified Data.Text as T

main :: IO ()
main = do
    let minhaString = T.pack "Olá, Haskell!"
    print $ T.length minhaString
```

## See Also
Para explorar mais, confira:

- [Haskell Documentation for Lists](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html)
- [Data.Text package on Hackage](https://hackage.haskell.org/package/text)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters) - Um guia introdutório para Haskell com uma abordagem divertida.
