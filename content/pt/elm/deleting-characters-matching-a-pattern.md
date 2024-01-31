---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:02.952252-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Deletar caracteres que seguem um padrão específico é basicamente filtrar texto. Programadores fazem isso para limpar dados, validar input ou simplificar strings antes de processá-las.

## Como fazer:
```Elm
import String exposing (filter)
import Regex exposing (fromString, contains)

removePattern : String -> String -> String
removePattern pattern text =
    case fromString pattern of
        Nothing ->
            text

        Just regex ->
            filter (\char -> not (contains regex (String.fromChar char))) text

-- Usando a função:
removePattern "[0-9]" "Elm2023 é top!"          --> "Elm é top!"
removePattern "[^A-Za-z\\s]" "Olá, Mundo!123."  --> "Olá Mundo"
```

## Aprofundando
Historicamente, manipulação de strings é crítica em programação e cada linguagem tem sua abordagem. Em Elm, Regex e a função `filter` da biblioteca `String` são usadas para essa tarefa. Alternativas incluem escrever a própria função de filtragem ou usar funções de alto nível da biblioteca `String.Extra`. A implementação em Elm favorece clareza e segurança de tipos ao invés da performance pura; cada caractere é verificado individualmente contra o padrão regex, efetivamente deixando de fora aqueles que correspondem.

## Veja Também
- Elm String Documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Regex Documentation: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- Artigo "Working with Strings in Elm": https://medium.com/@_rchaves_/working-with-strings-in-elm-803c9dd521c7
- Elm String.Extra Library: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
