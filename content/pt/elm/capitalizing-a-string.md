---
title:                "Maiúsculas em uma string"
html_title:           "Elm: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê? 

Capitalizar uma string é simplesmente transformar a primeira letra de cada palavra em maiúscula. Programadores geralmente fazem isso para melhorar a legibilidade de seus códigos, tornando-os mais fáceis de entender.

## Como Fazer:

Em Elm, existem várias maneiras de capitalizar uma string. Uma delas é usando a função `String.capitalize` que transformará a primeira letra maiúscula e deixará todas as outras letras minúsculas. Outra opção é utilizar a biblioteca `elm-community/elm-string-extras` que oferece a função `String.Extra.capitalizeWords` que capitaliza todas as palavras em uma string.

```Elm
import String exposing (capitalize)
import String.Extra exposing (capitalizeWords)

capitalizado = capitalize "olá mundo"
-- Output: "Olá mundo"

todasPalavrasCapitalizadas = capitalizeWords "bem-vindo ao elm"
-- Output: "Bem-Vindo Ao Elm"
```

## Mergulho Profundo:

A prática de capitalizar strings vem dos primeiros dias da programação, quando as máquinas eram limitadas em termos de memória e recursos. Naquela época, era comum escrever códigos sem espaços e com todas as letras em maiúsculas. Atualmente, capitalizar strings é uma forma de deixar o código mais legível e também pode ser usada para uniformizar a formatação de dados.

Existem outras maneiras de capitalizar uma string em Elm, como por exemplo escrevendo sua própria função para isso. Seria algo parecido com isso:

```Elm
import List exposing (head, map)
import Char exposing (toUpper)

capitalizeAll words =
    String.join " " (map capitalizeWord words)

capitalizeWord word =
    String.fromList (
        toUpper (head word) :: List.repeat (String.toLower (head word)) (String.length word - 1)
    )
```

## Veja Também:

- Documentação oficial do `String` em Elm (https://package.elm-lang.org/packages/elm/core/latest/String)
- Biblioteca `elm-community/elm-string-extras` (https://package.elm-lang.org/packages/elm-community/elm-string-extras/latest)
- Outra abordagem para capitalizar strings em Elm (https://discourse.elm-lang.org/t/capitalize-first-letter-learning-experience/1136)