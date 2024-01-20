---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Encontrar o tamanho de uma string significa calcular quantos caracteres ela contém. Programadores fazem isso para saber o tamanho da informação sendo manipulada, o que impacta diretamente no controle do fluxo dentro de um sistema.

## Como Fazer:
Aqui está um exemplo de como encontrar o tamanho de uma string no Elm:

```Elm
import String exposing (length)

main =
  let
    minhaString = "Programação no Elm é legal demais."
  in
  length minhaString
```
Quando você roda este programa, o valor gerado será o tamanho da string, que neste caso é 35.

## Mergulho Profundo
O método `length` em Elm, usado para encontrar o comprimento de uma string, é bastante antigo na história das linguagens de programação. Ele é um dos métodos fundamentais encontrados em qualquer linguagem que manipula strings.

Um caminho alternativo poderia ser processar a string como uma lista de caracteres e, então, contar os elementos. Mas isso seria desnecessariamente complicado e menos eficiente. Com o Elm adotando um design funcional, o mais natural é tratar as strings de maneira semelhante às listas, porém, já existe um método dedicado para calcular a length.

A implementação interna de `length` em Elm trabalha contando cada elemento unitário em uma string. Um fato importante é que `length` trata corretamente caracteres Unicode, graças à maneira como Elm manipula internamente as strings.

## Veja Também:
1. Documentação do Elm sobre a função String.length: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
2. Discussão sobre eficiência de String.length: [https://discourse.elm-lang.org/t/string-length-efficiency/5578](https://discourse.elm-lang.org/t/string-length-efficiency/5578)