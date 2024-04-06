---
date: 2024-01-20 17:45:35.679302-07:00
description: "Como Fazer: A extra\xE7\xE3o de substrings n\xE3o \xE9 algo novo; vem\
  \ desde os tempos das primeiras linguagens de programa\xE7\xE3o. Em Elm, utilizamos\
  \ fun\xE7\xF5es como\u2026"
lastmod: '2024-04-05T22:50:59.748285-06:00'
model: gpt-4-1106-preview
summary: "A extra\xE7\xE3o de substrings n\xE3o \xE9 algo novo; vem desde os tempos\
  \ das primeiras linguagens de programa\xE7\xE3o."
title: Extraindo substrings
weight: 6
---

## Como Fazer:
```Elm
import String exposing (slice)

-- Vamos assumir que temos a seguinte string:
let
    texto = "Programação em Elm é elegante e robusta."
in
-- Para pegar a palavra "Elm":
String.slice 16 19 texto -- "Elm"

-- E se quisermos a frase "elegante e robusta"?
String.slice 22 39 texto -- "elegante e robusta"
```

Saída esperada:
```
"Elm"
"elegante e robusta"
```

## Mergulho Profundo
A extração de substrings não é algo novo; vem desde os tempos das primeiras linguagens de programação. Em Elm, utilizamos funções como `String.slice`, que é bem direta e faz o trabalho de maneira eficiente. Alternativas em outras linguagens incluem funções como `substring` ou métodos como `substr`. Mas, no Elm, ainda não temos uma função integrada chamada `substring`; `String.slice` é o padrão. A implementação de extrair substrings é uma funçao pura em Elm, ou seja, dadas as mesmas entradas, sempre terá a mesma saída, sem efeitos colaterais.

## Veja Também
- Documentação oficial da função `String.slice`: [Elm String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- Elm Lang: Uma introdução para iniciantes, onde substrings podem ser discutidas: [An Introduction to Elm](https://guide.elm-lang.org/)
