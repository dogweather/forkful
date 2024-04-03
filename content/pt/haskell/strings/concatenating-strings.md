---
date: 2024-01-20 17:34:55.212749-07:00
description: "Como Fazer: Concatenar strings em Haskell \xE9 simples. Voc\xEA pode\
  \ usar o operador `++` ou a fun\xE7\xE3o `concat`. Aqui est\xE3o alguns exemplos."
lastmod: '2024-03-13T22:44:46.617455-06:00'
model: gpt-4-1106-preview
summary: "Concatenar strings em Haskell \xE9 simples."
title: Concatenando strings
weight: 3
---

## Como Fazer:
Concatenar strings em Haskell é simples. Você pode usar o operador `++` ou a função `concat`. Aqui estão alguns exemplos:

```haskell
main :: IO ()
main = do
  let hello = "Olá"
  let world = "Mundo"
  
  -- Usando o operador ++
  putStrLn (hello ++ " " ++ world) -- "Olá Mundo"
  
  -- Usando concat
  putStrLn (concat [hello, " ", world]) -- "Olá Mundo"
```

## Mergulho Profundo:
Historicamente, a concatenação de strings é uma operação básica na maioria das linguagens de programação, e Haskell não é exceção. Alternativas para concatenação em Haskell incluem o uso de `StringBuilder` em ambientes onde a performance é crítica ou até mesmo o uso de `intercalate` de `Data.List` se você estiver lidando com uma lista de strings.

A implementação da concatenação em Haskell é otimizada para ser tão eficiente quanto possível, mas é importante lembrar que o operador `++` tem complexidade O(n) em relação ao comprimento da primeira lista, então use-o sabiamente.

## Veja Também:
Para mais detalhes e exemplos sobre manipulação de strings em Haskell, confira os seguintes links:

- [Haskell Wiki sobre strings](https://wiki.haskell.org/Strings)
- [Hackage: pacote `text`](https://hackage.haskell.org/package/text)
- [LYAH: Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
