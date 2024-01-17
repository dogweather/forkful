---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Haskell: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# O que & Por quê?
Remover caracteres que correspondem a um padrão é uma tarefa comum na programação, em que os programadores precisam realizar mudanças em uma string ou sequência de texto. Essa técnica é utilizada para remover caracteres específicos que não são necessários ou que não devem estar presentes na sequência.

# Como fazer:
```Haskell
-- Exemplo 1: Removendo vogais de uma string
removeVogais :: String -> String
removeVogais "" = ""
removeVogais (x:xs) 
  | (x `elem` "aeiouAEIOU") = removeVogais xs
  | otherwise = x:removeVogais xs

-- Exemplo 2: Removendo caracteres de pontuação
removePontuacao :: String -> String
removePontuacao "" = ""
removePontuacao (x:xs)
  | (x `elem` ",.?!") = removePontuacao xs
  | otherwise = x:removePontuacao xs
```

# Profundando:
A remoção de caracteres correspondentes a um padrão está presente em várias linguagens de programação, não apenas em Haskell. Em outras linguagens, é comum o uso de expressões regulares para realizar essa tarefa. Em Haskell, as funções de alta ordem, como ```filter``` e ```map```, também podem ser utilizadas para remover caracteres de acordo com um determinado critério.

# Veja também:
- [Documentação da função filter em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:filter)
- [Tutoriais sobre expressões regulares em outras linguagens](https://www.regular-expressions.info/)