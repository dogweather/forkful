---
title:                "Elm: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que usar o Elm para procurar e substituir texto?

Procurar e substituir texto é uma tarefa comum no desenvolvimento de software. Com o Elm, você pode automatizar esse processo e economizar tempo e esforço. Além disso, o Elm é uma linguagem de programação funcional elegante e robusta, que oferece uma maneira simples e eficiente de manipular strings.

## Como fazer isso no Elm:

Em primeiro lugar, precisamos declarar uma string que contenha o texto que desejamos modificar. Usaremos o operador `~` para atribuir o valor a uma variável.

```Elm
meuTexto = "Eu gosto de programar em Elm."
```

Em seguida, iremos utilizar a função `replace` do módulo `String` para substituir "gosto" por "amo" no texto:

```Elm
novoTexto = String.replace "gosto" "amo" meuTexto
```

E se quisermos procurar e substituir em todas as ocorrências da string? Podemos usar a função `replace` juntamente com a função `words`, que divide o texto em uma lista de palavras. Em seguida, podemos usar o operador `|>` para encadear as funções e realizar a substituição em cada elemento da lista.

```Elm
novaLista = meuTexto
  |> String.words
  |> List.map (\word -> String.replace "gosto" "amo" word)
  |> String.join " "
  |> String.trim
```

Na linha 4, estamos unindo a lista de palavras de volta em uma única string, e na linha 5, estamos removendo espaços em branco extras do início e do fim da string.

O resultado final será: "Eu amo de programar em Elm."

## Aprofundando-se no assunto:

Além da função `replace`, o módulo `String` do Elm oferece outras funções úteis para manipulação de strings, como `startsWith`, `endsWith` e `contains`. Além disso, a linguagem Elm facilita a escrita de expressões lambda (funções anônimas) para realizar operações mais complexas em strings.

Com o uso de bibliotecas externas, como o módulo `elm-regex`, é possível realizar substituições com expressões regulares e criar regras avançadas para manipulação de texto.

## Veja também:

- [Documentação oficial do módulo String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Módulo elm-regex](http://package.elm-lang.org/packages/elm-community/regex/latest)