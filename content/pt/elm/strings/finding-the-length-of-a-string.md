---
date: 2024-01-20 17:47:21.121855-07:00
description: "Como Fazer: Historicamente, a fun\xE7\xE3o para determinar o comprimento\
  \ de uma string sempre foi essencial em linguagens de programa\xE7\xE3o. No Elm,\u2026"
lastmod: '2024-04-05T21:53:46.830282-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a fun\xE7\xE3o para determinar o comprimento de uma string\
  \ sempre foi essencial em linguagens de programa\xE7\xE3o."
title: Descobrindo o comprimento de uma string
weight: 7
---

## Como Fazer:
```Elm
import String

main =
    let
        frase = "Olá, mundo!"
    in
    String.length frase
-- Saída: 12
```

```Elm
import String

contaCaracteres : String -> Int
contaCaracteres texto = String.length texto

-- Uso:
resultado = contaCaracteres "Elm é bacana!"
-- Saída: 13
```

## Aprofundamento
Historicamente, a função para determinar o comprimento de uma string sempre foi essencial em linguagens de programação. No Elm, `String.length` é a maneira direta de fazer isso. Alternativas podem envolver a criação de funções customizadas que talvez processem a string de forma diferente, como ignorando espaços ou caracteres especiais, mas isso geralmente não é necessário. Sob o capô, `String.length` conta os pontos de código Unicode, o que significa que emojis ou caracteres especiais podem ser contabilizados de maneira inesperada, uma vez que podem ser compostos de múltiplos pontos de código.

## Veja Também
- Documentação oficial da Elm para a função `String.length`: [String.length](http://package.elm-lang.org/packages/elm-lang/core/latest/String#length)
- Tutorial de Elm para iniciantes: [Elm Tutorial for Beginners](https://guide.elm-lang.org/)
- Comunidade Elm no Reddit: [r/elm](https://www.reddit.com/r/elm/)
