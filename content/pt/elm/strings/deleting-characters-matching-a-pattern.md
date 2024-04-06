---
date: 2024-01-20 17:42:02.952252-07:00
description: "Como fazer: Historicamente, manipula\xE7\xE3o de strings \xE9 cr\xED\
  tica em programa\xE7\xE3o e cada linguagem tem sua abordagem. Em Elm, Regex e a\
  \ fun\xE7\xE3o `filter` da\u2026"
lastmod: '2024-04-05T21:53:46.823019-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, manipula\xE7\xE3o de strings \xE9 cr\xEDtica em programa\xE7\
  \xE3o e cada linguagem tem sua abordagem."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

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
