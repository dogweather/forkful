---
title:                "Descobrindo o comprimento de uma string"
aliases:
- pt/elm/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:21.121855-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Encontrar o comprimento de uma string é basicamente contar quantos caracteres ela possui. Programadores fazem isso para validar entradas, limitar tamanhos, entre outras coisas que dependem da quantidade exata de caracteres.

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
