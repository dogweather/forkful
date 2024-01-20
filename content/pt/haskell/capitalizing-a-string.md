---
title:                "Capitalizando uma string"
html_title:           "Haskell: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizando Strings em Haskell

## O Que & Por Quê?
Capitalizar uma string significa transformar a primeira letra de cada palavra em maiúscula. Programadores usam essa operação frequentemente para formatar textos adequadamente, especialmente para UIs (Interfaces de Usuário) e apresentações de dados.

## Como Fazer:
Em Haskell, uma função básica para capitalizar uma string pode ser escrita usando a biblioteca de "Data.Char".

```Haskell
import Data.Char (toUpper)

capitalizarString :: String -> String
capitalizarString "" = ""
capitalizarString (x:xs) = toUpper x : xs
```
Ao testarmos com a string "olá, mundo!", teremos:

```Haskell
main = putStrLn (capitalizarString "olá, mundo!") -- Impressão: "Olá, mundo!"
```
## Aprofundando
Haskell, sendo uma linguagem funcional, oferece uma abordagem distinta e elegante para capitalizar strings. A função `toUpper` da biblioteca de `Data.Char` converte um caractere em letra maiúscula.

Alternativas para a mesma operação poderiam incluir o uso da função `map` para aplicar `toUpper` em cada palavra de uma lista de palavras, que por sua vez é produzida pela função pré-definida `words`. No entanto, essa abordagem capitalizaria todas as letras, não apenas a primeira de cada palavra.

Quanto à implementação, numa visão mais aprofundada, o código `(x:xs)` usa pattern matching para distinguir o primeiro caractere (`x`) e o restante da string (`xs`). Assim, apenas o primeiro caractere é transformado em maiúsculo.

## Veja Também: 
1. Biblioteca [Haskell `Data.Char`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)