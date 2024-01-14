---
title:                "Elm: Excluindo caracteres correspondentes a um padrão"
simple_title:         "Excluindo caracteres correspondentes a um padrão"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Quem trabalha com programação já deve ter se deparado com alguma tarefa que envolva a remoção de caracteres de uma string que correspondam a um determinado padrão. Embora possa parecer uma tarefa simples, ter ferramentas para lidar com esse tipo de situação pode tornar o seu código mais eficiente e elegante.

## Como fazer

A linguagem de programação Elm oferece uma forma simples e direta de lidar com a remoção de caracteres correspondentes a um padrão. Para isso, podemos utilizar o módulo `String` e sua função `removeAll` para remover todas as ocorrências de um determinado caractere de uma string.

Veja um exemplo abaixo de como podemos utilizar essa função:

```Elm
import String exposing (removeAll)

removeChar : String -> Char -> String
removeChar str char =
    removeAll (String.fromChar char) str
```

Neste exemplo, temos uma função chamada `removeChar` que recebe uma string e um caractere como argumentos e utiliza a função `removeAll` para remover todas as ocorrências desse caractere na string. Observe também que é necessário converter o caractere em uma string utilizando a função `String.fromChar` antes de passá-lo como argumento para `removeAll`.

Podemos testar essa função com alguns exemplos:

```Elm
removeChar "banana" 'a' -- retorna "bnn"
removeChar "elm rocks" ' ' -- retorna "elmrocks"
removeChar "hello world" 'l' -- retorna "heo word"
```

## Uma análise mais aprofundada

Além da função `removeAll`, o módulo `String` também oferece outras funções úteis para lidar com a manipulação de strings, como por exemplo a função `filter` que retorna uma lista de todos os caracteres de uma string que correspondam a um determinado predicado. Podemos utilizar essa função em conjunto com `removeAll` para criar uma função mais genérica de remoção de caracteres.

Veja o exemplo abaixo:

```Elm
import String exposing (filter, removeAll)

removeChars : String -> (Char -> Bool) -> String
removeChars str pred =
    removeAll (String.fromList (filter pred str)) str
```

Neste exemplo, a função `removeChars` recebe uma string e uma função de predicado como argumentos e utiliza `filter` para obter uma lista dos caracteres que correspondem ao predicado e então usa `removeAll` para removê-los da string.

Isso nos permite utilizar qualquer função de predicado que desejemos, como por exemplo:

```Elm
removeChars "elm is awesome" Char.isUpper -- retorna "lm is awesome"
removeChars "9876543210" Char.isDigit -- retorna ""
```

## Veja também

- [Documentação do módulo `String` no website do Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Outras funções úteis para manipulação de strings no Elm](https://guide.elm-lang.org/strings/)

Muitas vezes, pequenas tarefas como a remoção de caracteres podem ser resolvidas com funções simples, mas é importante conhecer as ferramentas disponíveis na linguagem que estamos utilizando para escrever código mais eficiente e legível. Esperamos que este artigo tenha sido útil e que você possa aplicá-lo em seus projetos com Elm.