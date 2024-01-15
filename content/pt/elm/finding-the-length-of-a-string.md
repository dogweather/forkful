---
title:                "Encontrando o comprimento de uma string"
html_title:           "Elm: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que 

Se você é um programador em Elm, provavelmente já se deparou com a necessidade de encontrar o comprimento de uma string em algum momento. Entender como fazer isso é essencial para trabalhar com dados de texto e criar lógica eficiente em seus programas.

## Como Fazer

Encontrar o comprimento de uma string é algo relativamente simples em Elm. Tudo o que você precisa fazer é chamar a função `String.length` e passar a string como argumento. Veja um exemplo de código abaixo:

```Elm
idade = "20 anos"
String.length idade
```

A saída desse código será `9`, pois a string possui um total de nove caracteres. Você também pode usar essa função em conjunto com outras funções de manipulação de strings para criar lógica mais avançada.

## Deep Dive

A função `String.length` é uma função de ordem superior em Elm, o que significa que ela recebe outra função como argumento. Isso permite que você personalize ainda mais a lógica de encontrar o comprimento de uma string.

Uma outra abordagem para encontrar o comprimento de uma string é usar a função `List.length`. Como as strings são listas de caracteres em Elm, você pode usar essa função e passar a string como argumento. Veja um exemplo de código abaixo:

```Elm
idade = "20 anos"
List.length (String.toList idade)
```

A saída desse código será novamente `9`, pois a string foi convertida em uma lista de caracteres para que a função `List.length` possa ser aplicada. Isso pode ser útil quando você precisar realizar manipulações mais específicas em cada caractere da string.

## Veja Também

- Documentação oficial do Elm: https://elm-lang.org/docs
- Tutorial de Elm para iniciantes: https://guide.elm-lang.org/
- Funções de manipulação de strings em Elm: https://package.elm-lang.org/packages/elm/core/latest/String