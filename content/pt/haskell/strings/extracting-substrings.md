---
date: 2024-01-20 17:46:07.683145-07:00
description: "Extrair substrings significa pegar partes espec\xEDficas de uma string.\
  \ Programadores fazem isso para manipular, analisar ou transformar dados baseados\
  \ em\u2026"
lastmod: '2024-03-11T00:14:20.324850-06:00'
model: gpt-4-1106-preview
summary: "Extrair substrings significa pegar partes espec\xEDficas de uma string.\
  \ Programadores fazem isso para manipular, analisar ou transformar dados baseados\
  \ em\u2026"
title: Extraindo substrings
---

{{< edit_this_page >}}

## O Quê & Porquê?
Extrair substrings significa pegar partes específicas de uma string. Programadores fazem isso para manipular, analisar ou transformar dados baseados em texto de maneira eficiente.

## Como Fazer:
```Haskell
import Data.List (isPrefixOf)

-- Exemplo 1: Usar a função 'take'
substringInicio :: String -> Int -> String
substringInicio s n = take n s

-- Exemplo 2: Usar a função 'drop'
substringFinal :: String -> Int -> String
substringFinal s n = drop n s

-- Exemplo 3: Usando 'take' e 'drop' juntas para pegar uma substring no meio
substringMeio :: String -> Int -> Int -> String
substringMeio s inicio tamanho = take tamanho . drop inicio $ s 

-- Resultados
main :: IO ()
main = do
    putStrLn $ substringInicio "Olá mundo!" 4 -- "Olá "
    putStrLn $ substringFinal "Haskell" 3     -- "ell"
    putStrLn $ substringMeio "Fatiar strings" 7 6 -- "string"
```

## Mergulho Profundo
Extrair substrings é uma operação básica, mas fundamental, das linguagens de programação desde o início, porque processar texto é incrivelmente comum. Em Haskell, a abordagem puramente funcional tem algumas implicações na forma como lidamos com strings. Em vez de métodos internos que alteram a string original (como algumas outras linguagens fazem), Haskell favorece o uso de funções que retornam novas strings.

Alternativas para extrair substrings podem incluir expressões regulares (com o pacote `regex`), que são mais flexíveis mas também mais complexas. Uma outra característica a se considerar é quando trabalhamos com textos grandes. O tipo `String` em Haskell é representado como uma lista de caracteres, o que pode ser ineficiente para strings grandes. Nesses casos, pode ser melhor usar a biblioteca `Text` que é otimizada para lidar com texto de forma mais performática.

## Veja Também
- [Haskell `take` function](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:take)
- [Haskell `drop` function](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:drop)
- Documentação do pacote Text: [Text Haskell package](https://hackage.haskell.org/package/text)
- Expressões regulares em Haskell: [regex Haskell packages](https://hackage.haskell.org/package/regex-base)
