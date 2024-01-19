---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e Por quê?
Interpolação de string é uma maneira de incluir valores de variáveis dentro de uma string. Programadores o fazem para tornar o código mais legível e para formatar saída de dados.

## Como fazer:
Elm não tem suporte nativo para a interpolação de strings, mas pode ser feita facilmente com a concatenação de strings, usando o operador `(++)`. Veja isso:

```Elm
nome = "João"
mensagem = "Olá, " ++ nome

-- Saída: "Olá, João"
```

A alternativa é usar a função `String.concat` que aceita uma lista de strings:

```Elm
parte1 = "Olá, "
parte2 = "João"
mensagem = String.concat [parte1, parte2]

-- Saída: "Olá, João"
```

## Mergulho Profundo
Históricamente, Elm prioriza a acessibilidade do código e a segurança em relação à simplificação sintática, é por isso que disge a interpolação de strings. Em termos de alternativas, nas versões anteriores de Elm, usavam-se bibliotecas de terceiros que forneciam funcionalidades de formatação de string. No entanto, com o tempo, as atualizações do Elm desencorajaram esse método em favor da concatenação de string.

Em termos de implementação, as duas maneiras de "interpolating" uma string em Elm - usando a concatenação ou `String.concat` - têm eficiências quase idênticas. O primeiro é mais simples de usar para strings curtas e o último é mais adequado para listas grandes de strings.

## Veja Também
Para mais discussões e melhores práticas com strings em Elm, consulte os seguintes links:

1. Documentação oficial do Elm para strings: https://package.elm-lang.org/packages/elm/core/1.0.5/String
2. Discussão no Reddit sobre interpolação de strings em Elm: https://www.reddit.com/r/elm/comments/4l6vma/string_interpolation_in_elm/
3. Postagem de blog detalhada sobre a manipulação de strings em Elm: https://elmprogramming.com/strings.html