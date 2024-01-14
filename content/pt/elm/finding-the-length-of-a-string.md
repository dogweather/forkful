---
title:                "Elm: Encontrando o comprimento de uma sequência"
simple_title:         "Encontrando o comprimento de uma sequência"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com programação, muitas vezes precisamos manipular e analisar cadeias de caracteres (strings). Uma tarefa comum é encontrar o comprimento (length) de uma string, que pode ser útil em diversas situações, como validação de formulários e manipulação de dados.

## Como fazer

Para encontrar o comprimento de uma string em Elm, utilizamos a função `String.length`. Veja um exemplo abaixo:

```Elm
nomeCompleto : String
nomeCompleto = "João da Silva"

tamanhoNome : Int
tamanhoNome = String.length nomeCompleto

-- O resultado será 14, já que a string tem 14 caracteres
```

É importante lembrar que, em Elm, a função `String.length` retorna um valor do tipo `Int`. Portanto, se quisermos usar o resultado em uma expressão, devemos converter para `Float` usando a função `toFloat` ou arredondar para baixo usando `floor`.

## Mergulho Profundo

Ao utilizar a função `String.length`, é importante ter em mente que ela conta o número de caracteres, e não o número de palavras. Por exemplo, se tivermos a string `"casa de praia"`, o resultado será 13, pois há um espaço em branco entre as palavras.

Além disso, a função `String.length` também conta caracteres especiais, como acentos e cedilhas. Portanto, caso precise contar apenas as letras, é necessário remover esses caracteres antes de encontrar o comprimento da string.

## Veja também

- Documentação oficial sobre a função `String.length` em Elm: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Como remover caracteres especiais de uma string em Elm: https://stackoverflow.com/questions/33665026/remove-special-characters-in-a-string-with-elm
- Como converter de `Int` para `Float` em Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics#toFloat
- Como arredondar um número para baixo em Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics#floor