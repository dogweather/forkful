---
title:                "Buscando e substituindo texto"
html_title:           "Elm: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos fazer alterações em grandes quantidades de texto. Em vez de procurar e substituir manualmente, podemos usar o recurso de busca e substituição, que nos permite automatizar essa tarefa e economizar tempo e esforço.

## Como Fazer

```Elm
main :
    String
main =
    "Hello, world!"
```

Para realizar uma busca e substituição em Elm, podemos usar a função `replace` do módulo `String`. Precisamos especificar a string que desejamos modificar, o texto que queremos substituir e o texto que será usado como substituto. Por exemplo:

```Elm
import String

myString : String
myString =
    "Aqui tem uma palavra que queremos substituir"

novoString : String
novoString =
    String.replace "queremos" "substituir" myString
```

O resultado será "Aqui tem uma palavra que substituir substituir". Podemos também usar a função `replaceAll` se quisermos substituir todas as ocorrências de uma determinada palavra ou expressão. Por exemplo:

```Elm
import String

myString : String
myString =
    "Esta é uma string com várias ocorrências de 'maçã'"

novoString : String
novoString =
    String.replaceAll "maçã" "banana" myString
```

O resultado será "Esta é uma string com várias ocorrências de 'banana'".

## Mergulho Profundo

A função `replace` do módulo `String` é definida da seguinte forma:

```Elm
replace : String -> String -> String -> String
```

Isso significa que ela recebe três argumentos do tipo `String` e retorna uma nova `String` como resultado. O primeiro argumento é a string que queremos modificar, o segundo é o texto que desejamos substituir e o terceiro é o texto que será usado como substituto.

Além disso, a função `replaceAll` também está disponível no módulo `String` e é definida como:

```Elm
replaceAll : String -> String -> String -> String
```

A única diferença é que essa função substitui todas as ocorrências da palavra ou expressão, enquanto a função `replace` substitui apenas a primeira ocorrência.

## Veja Também

- Documentação oficial do módulo `String` em Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- Tutorial sobre busca e substituição em Elm: https://dev.to/charlottebrf/string-replace-in-elm-1bhf