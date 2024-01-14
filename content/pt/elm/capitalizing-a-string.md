---
title:                "Elm: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Elm?

A capitalização de strings é uma tarefa comum em muitas linguagens de programação, incluindo o Elm. Ao capitalizar uma string, estamos transformando cada letra inicial em maiúscula, tornando-a mais legível e esteticamente agradável. Portanto, capitalizar uma string é importante para garantir que nossos dados sejam apresentados da melhor maneira possível para os usuários.

## Como fazer em Elm

Em Elm, podemos capitalizar uma string usando a função `String.toUpper` e `String.toLower`. Vamos dar uma olhada em alguns exemplos de código para entender melhor como essas funções funcionam.

```Elm
captalizeString : String -> String
captalizeString str =
  String.toUpper (String.left 1 str) ++ String.dropLeft 1 str

--- Exemplo de entrada e saída
captalizeString "elm" -- "Elm"
captalizeString "linguagem de programação" -- "Linguagem de programação"
```

Neste exemplo, usamos a função `String.toUpper` para transformar a primeira letra da string em maiúscula. Em seguida, usamos `String.dropLeft` para remover a primeira letra não capitalizada e, finalmente, concatenamos as duas partes para obter a string com a primeira letra maiúscula.

Também podemos usar a função `String.words` para dividir a string em palavras e, em seguida, capitalizar a primeira letra de cada palavra. Veja o exemplo abaixo:

```Elm
captalizeWords : String -> String
captalizeWords str =
  String.join " " (List.map (\word -> String.toUpper (String.left 1 word) ++ String.dropLeft 1 word) (String.words str))

--- Exemplo de entrada e saída
captalizeWords "linguagem de programação" -- "Linguagem De Programação"
```

Por fim, também podemos usar a biblioteca `elm-community/elm-string-extra`, que fornece uma função `toTitleCase` para capitalizar adequadamente strings de acordo com regras de título. Veja um exemplo de uso abaixo:

```Elm
import String.Extra exposing (toTitleCase)

capitalize : String -> String
capitalize str =
  toTitleCase str

--- Exemplo de entrada e saída
capitalize "elm community" --"Elm Community"
capitalize "linguagem de programação" -- "Linguagem de Programação"
```

## Mergulhando mais fundo

Ao capitalizar uma string em Elm, é importante ter em mente que a função `String.toUpper` e `String.toLower` apenas alteram a primeira letra da string. Portanto, se a string contiver caracteres especiais ou números antes da primeira letra, esses caracteres não serão maiúsculos.

Além disso, para strings que contêm diacríticos, como acentos ou cedilhas, é preciso ter cuidado ao usarmos a função `String.toUpper` e `String.toLower`. Nestes casos, é recomendado usar a função `toTitleCase` da biblioteca `elm-community/elm-string-extra` para garantir a capitalização correta.

## Veja também

- [Documentação oficial da linguagem Elm](https://elm-lang.org/docs)
- [elm-string-extra](https://package.elm-lang.org/packages/elm-community/elm-string-extra/latest/)
- [Artigo sobre capitalização de strings em Elm](https://medium.com/@lorenzovillani/strings-prepocessing-with-elm-8c398712f324)