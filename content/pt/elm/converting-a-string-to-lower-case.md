---
title:    "Elm: Convertendo uma string para minúsculas"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que Converter uma String para Minúsculas?

Quando estamos programando em Elm, muitas vezes precisamos manipular strings. Uma tarefa comum é converter uma string para minúsculas. Isso pode ser útil em várias situações, como quando queremos comparar duas strings de forma não sensível a maiúsculas e minúsculas ou quando queremos formatar uma string para exibi-la em letras minúsculas.

## Como Fazer

A linguagem Elm possui uma função nativa chamada `String.toLower` que permite converter uma string para minúsculas facilmente, como mostrado no exemplo abaixo:

```Elm
nomeEmMaiusculo = "JOANA"
nomeEmMinusculo = String.toLower nomeEmMaiusculo

nomeEmMinusculo -- saída: "joana"
```

Podemos também usar essa função para comparar duas strings independentemente de suas letras maiúsculas ou minúsculas. Por exemplo:

```Elm
nome1 = "Laura"
nome2 = "laura"
nome3 = "LAURA"

String.toLower nome1 == String.toLower nome2 -- saída: True
String.toLower nome1 == String.toLower nome3 -- saída: True
```

## Mergulho Nas Profundezas

Por baixo dos panos, a função `String.toLower` utiliza o Unicode para converter a string para minúsculas. Isso significa que ela é capaz de lidar com caracteres acentuados e de outras línguas. Além disso, ela também é capaz de converter letras maiúsculas acentuadas para suas equivalentes minúsculas sem acentos.

Por exemplo, a letra "Á" será convertida para "á", a letra "Ç" será convertida para "ç" e assim por diante. Isso torna a função `String.toLower` muito poderosa e versátil para lidar com strings em diferentes idiomas.

## Veja Também

- [Documentação oficial da função `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Exemplos de uso da função `String.toLower`](https://elmprogramming.com/convert-string-to-lowercase-in-elm.html)
- [Outras funções de manipulação de strings em Elm](https://courses.knowthen.com/p/pure-functional-data-structures-in-elm/?product_id=79614&coupon_code=YT50OFF&preview=logged_out) (em inglês)