---
title:    "Haskell: Concatenando strings"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que 

Concatenar strings é uma habilidade essencial para qualquer programador Haskell. Combinando diferentes strings, podemos criar mensagens personalizadas, formatar dados de maneira mais legível e construir novas funcionalidades para nossos programas.

## Como Fazer

```Haskell
concatenar :: String -> String -> String
concatenar x y = x ++ y

mensagem1 = "Olá, "
mensagem2 = "mundo!"
concatenar mensagem1 mensagem2
--Output: "Olá, mundo!"

--Também podemos concatenar mais de duas strings:
mensagem3 = "Hoje é "
mensagem4 = "um dia "
mensagem5 = "ensolarado."
concatenar (concatenar mensagem3 mensagem4) mensagem5
--Output: "Hoje é um dia ensolarado."

--Podemos até mesmo combinar strings com outros tipos de dados, desde que usemos a função "show":
idade = 25
concatenar "Eu tenho " (show idade) ++ " anos."
--Output: "Eu tenho 25 anos."
```

## Mergulho Profundo

Em Haskell, a concatenação de strings é feita através do operador "++". Ele simplesmente combina duas strings em uma, sem adicionar nenhum espaço ou caractere especial entre elas. No entanto, se precisarmos adicionar algum caractere entre as strings (como um espaço), é possível utilizar a função "intercalate", que recebe um delimitador e uma lista de strings para serem unidas.

Outra função útil para manipular strings é a "words", que divide uma string em uma lista de palavras. E a função "unwords" faz o contrário, unindo uma lista de palavras em uma única string, adicionando espaços entre elas.

Existem ainda outras funções relacionadas à manipulação de strings que podem ser exploradas, como "lines", "unlines", "take" e "drop".

## Veja Também

- [Haskell Wiki: Concatenação de Strings](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good - Stringy Pattern Matching](http://learnyouahaskell.com/chapters#stringy-pattern-matching)
- [Real World Haskell - Strings](http://book.realworldhaskell.org/read/strings.html)