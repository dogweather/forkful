---
title:                "Elm: Capitalização de uma string"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que usar uma função para capitalizar uma string?

Capitalizar uma string é um processo simples, mas que pode se tornar repetitivo ao longo do código. Além disso, pode ser necessário capitalizar apenas a primeira letra de uma string, o que pode exigir algumas comparações extras. Com uma função para capitalizar strings, esse trabalho fica mais fácil e rápido, economizando tempo e evitando erros.

## Como fazer isso em Elm

Para capitalizar uma string em Elm, podemos criar uma função que recebe uma string como argumento e retorna a mesma string com todas as letras maiúsculas. Podemos utilizar a função `toUpper` da biblioteca `String` para fazer a conversão. Veja um exemplo abaixo:

```elm
import String exposing (toUpper)

capitalize : String -> String
capitalize str = toUpper str
```

Se quisermos capitalizar apenas a primeira letra da string, podemos utilizar a função `head` da biblioteca `String` para pegar a primeira letra e depois concatená-la com o restante da string já capitalizado. Veja:

```elm
import String exposing (toUpper, head)

capitalizeFirst : String -> String
capitalizeFirst str =
   let
      firstLetter = head str
      rest = toUpper (String.dropLeft 1 str)
   in
      firstLetter ++ rest
```

Ao chamar a função `capitalizeFirst` com a string "elm", teremos como resultado a string "Elm".

## Aprofundando no processo de capitalização

Além das funções `toUpper` e `head`, que são essenciais para capitalizar strings em Elm, podemos utilizar outras funções da biblioteca `String` para deixar a capitalização mais personalizada. Por exemplo, podemos utilizar a função `toFloat` para transformar a string em um número e depois utilizá-lo como índice para pegar uma letra específica da string e capitalizá-la.

Além disso, também podemos utilizar variáveis locais e funções de manipulação de listas para capitalizar cada palavra individualmente em uma string, e não apenas a primeira letra.

## Veja também

- [Documentação da biblioteca String do Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Exemplo completo de capitalização de strings em Elm](https://github.com/example/elm-uppercase)
- [Tutorial sobre funções em Elm](https://blog.elm-lang.org/2018/06/28/an-introduction-to-elm-functions)