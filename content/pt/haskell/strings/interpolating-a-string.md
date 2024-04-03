---
date: 2024-01-20 17:50:54.364457-07:00
description: "Como Fazer: Haskell n\xE3o tem interpola\xE7\xE3o de strings nativa\
  \ como outras linguagens, mas podemos chegar l\xE1 com a biblioteca `text` e sua\
  \ fun\xE7\xE3o `printf` ou\u2026"
lastmod: '2024-03-13T22:44:46.611945-06:00'
model: gpt-4-1106-preview
summary: "Haskell n\xE3o tem interpola\xE7\xE3o de strings nativa como outras linguagens,\
  \ mas podemos chegar l\xE1 com a biblioteca `text` e sua fun\xE7\xE3o `printf` ou\
  \ a biblioteca `interpolate`."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
Haskell não tem interpolação de strings nativa como outras linguagens, mas podemos chegar lá com a biblioteca `text` e sua função `printf` ou a biblioteca `interpolate`. Aqui está como você pode usá-las:

```haskell
-- Usando a biblioteca text
import Text.Printf (printf)

main :: IO ()
main = do
    let nome = "Mundo"
    let boasVindas = printf "Olá, %s!" nome
    putStrLn boasVindas
    -- Saída: Olá, Mundo!

-- Usando a biblioteca interpolate
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let nome = "Mundo"
    let boasVindas = [i|Olá, #{nome}!|]
    putStrLn boasVindas
    -- Saída: Olá, Mundo!
```

## Aprofundando:
Historicamente, Haskell não incluiu a interpolação de string porque se baseia em uma abordagem minimalista e genérica para a manipulação de strings. Enquanto linguagens como Ruby ou JavaScript têm interpolação de string embutida, Haskell utiliza bibliotecas que proveem essa funcionalidade. Além de `printf` e `interpolate`, você pode usar bibliotecas alternativas como `formatting` para uma abordagem mais funcional e poderosa. A implementação envolve geralmente extensões da linguagem, como `OverloadedStrings` e quasiquoters para integrar sintaxe de interpolação na linguagem puramente funcional.

## Veja Também:
- [`printf` na Hackage](https://hackage.haskell.org/package/base-4.16.1.0/docs/Text-Printf.html)
- [Pacote `interpolate` na Hackage](https://hackage.haskell.org/package/interpolate)
- [Pacote `formatting` na Hackage](https://hackage.haskell.org/package/formatting)
