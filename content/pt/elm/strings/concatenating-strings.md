---
date: 2024-01-20 17:34:43.441174-07:00
description: "Como Fazer: Sa\xEDda esperada: \"Ol\xE1, Mundo!\"."
lastmod: '2024-04-05T21:53:46.831278-06:00'
model: gpt-4-1106-preview
summary: ''
title: Concatenando strings
weight: 3
---

## Como Fazer:
```Elm
import Html exposing (text)

main =
  let
    greeting = "Olá, "
    name = "Mundo!"
    message = greeting ++ name  -- Concatenação aqui
  in
  text message
```
Saída esperada: "Olá, Mundo!"

## Mergulho Profundo:
Historicamente, a concatenação de strings é um recurso que existe desde os primeiros dias da programação. Em Elm, a operação é realizada usando o operador `++`, uma escolha deliberada que difere de outras linguagens como JavaScript, que usa `+`. O motivo? Elm visa evitar as ambiguidades e os erros comuns que acontecem quando `+` é sobrecarregado para tipos numéricos e strings. Isso mantém o código claro e previsível. Alternativamente, você pode usar a função `String.concat`, que é útil quando você tem uma lista de strings para concatenar.

Exemplo com `String.concat`:
```Elm
import Html exposing (text)

main =
  text (String.concat ["Olá, ", "Mundo!"])
```
Saída esperada continua sendo: "Olá, Mundo!"

## Veja Também:
- Elm Language Guide on Strings: [Elm String Basics](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm Syntax Documentation: [Elm Syntax](https://elm-lang.org/docs/syntax)
- Elm String Concatenation Discussion: [Elm Discourse](https://discourse.elm-lang.org/)
