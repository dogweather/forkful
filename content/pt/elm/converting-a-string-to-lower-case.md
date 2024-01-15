---
title:                "Transformando uma string em minúsculas."
html_title:           "Elm: Transformando uma string em minúsculas."
simple_title:         "Transformando uma string em minúsculas."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
Há várias razões pelas quais alguém precisaria converter uma string para minúsculas em Elm. Uma das principais é garantir que os dados inseridos pelo usuário sejam consistentes e facilmente comparáveis.

## Como fazer
Fazer essa conversão é simples em Elm. Basta utilizar a função `String.toLower` e passar a string desejada como argumento. Veja um exemplo abaixo:

```elm
nome = "Mariana"
nomeLowercase = String.toLower nome
```

O output desse código seria `mariana`.

## Mergulho Profundo
Quando se trata de converter uma string para minúsculas em Elm, é importante entender como essa transformação ocorre. A função `String.toLower` utiliza a tabela Unicode para mapear as letras maiúsculas para suas respectivas minúsculas. Isso garante que caracteres acentuados e outros símbolos também sejam convertidos corretamente.

Outra coisa a se ter em mente é que, ao converter para minúsculas, a string original não é alterada. Em vez disso, é criada uma nova string com a versão em minúsculas. Isso é importante para entender quando lidamos com imutabilidade em Elm.

## Veja também
- [Documentação da função `String.toLower` em Elm](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Como comparar strings em Elm](https://medium.com/@ilyamilovidov/how-to-compare-strings-in-elm-%EF%B8%8F-8afad2c31bf)