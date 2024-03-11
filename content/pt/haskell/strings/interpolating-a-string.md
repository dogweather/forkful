---
date: 2024-01-20 17:50:54.364457-07:00
description: "Interpolar uma string significa inserir valores de vari\xE1veis ou express\xF5\
  es dentro dela para formar uma nova string. Programadores fazem isso para\u2026"
lastmod: '2024-03-11T00:14:20.322000-06:00'
model: gpt-4-1106-preview
summary: "Interpolar uma string significa inserir valores de vari\xE1veis ou express\xF5\
  es dentro dela para formar uma nova string. Programadores fazem isso para\u2026"
title: Interpolando uma string
---

{{< edit_this_page >}}

## O Que & Por Que?
Interpolar uma string significa inserir valores de variáveis ou expressões dentro dela para formar uma nova string. Programadores fazem isso para construir mensagens dinâmicas e customizadas sem a necessidade de concatenar pedaços de strings manualmente.

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
