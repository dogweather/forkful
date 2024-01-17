---
title:                "Maiúsculas em uma string"
html_title:           "Haskell: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Capitalizar uma string em Haskell significa converter as letras da string em maiúsculas. Os programadores muitas vezes fazem isso para padronizar o estilo de dados e facilitar o processamento e comparação de strings.

## Como fazer:

```
-- Utilizando a função toUpper da biblioteca Data.Char

import Data.Char

capitalizar :: String -> String
capitalizar str = map toUpper str

-- Exemplo de uso:

capitalizar "hello world" -- Saída: "HELLO WORLD"
```

```
-- Utilizando a função toUpper e um foldl para percorrer a string

import Data.Char

capitalizar :: String -> String
capitalizar = foldl (\acc x -> acc ++ [toUpper x]) ""

-- Exemplo de uso:

capitalizar "hello world" -- Saída: "HELLO WORLD"
```

```
-- Utilizando um list comprehension para percorrer e capitalizar cada letra

capitalizar :: String -> String
capitalizar str = [toUpper x | x <- str]

-- Exemplo de uso:

capitalizar "hello world" -- Saída: "HELLO WORLD"
```

## Profundidade:

Capitalizar strings é uma prática comum em programação e está presente em muitas linguagens de programação, não apenas em Haskell. Em Haskell, as strings são consideradas listas de caracteres, o que permite que sejam manipuladas com funções de lista, como o foldl utilizado no segundo exemplo acima. Alternativamente, poderíamos utilizar a função toTitle da biblioteca Data.Text para capitalizar apenas a primeira letra de cada palavra na string. A implementação da função toUpper utiliza a tabela ASCII para converter a letra para sua versão maiúscula correspondente.

## Veja também:

- [Hoogle](https://www.haskell.org/hoogle/) - uma ferramenta de busca para Haskell que permite pesquisar funções por tipo ou nome.
- [Página oficial de documentação do Haskell](https://www.haskell.org/documentation/) - para mais informações sobre a linguagem e suas bibliotecas padrões.