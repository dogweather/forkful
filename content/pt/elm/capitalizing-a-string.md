---
title:    "Elm: Capitalizando uma string"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Elm?

Se você já trabalhou com strings em Elm, provavelmente já se deparou com a necessidade de capitalizar uma palavra ou frase. Mas por que isso é importante? A resposta é simples: a capitalização correta é fundamental para a legibilidade e compreensão do texto, especialmente em aplicações que lidam com dados de usuários.

## Como capitalizar uma string em Elm

Felizmente, Elm possui uma função integrada para capitalizar uma string: `String.capitalize`. Basta fornecer a string desejada como argumento e a função irá retornar uma nova string com a primeira letra em maiúscula. Veja um exemplo abaixo:

```Elm
String.capitalize "amor"
-- Resultado: "Amor"
```

Você também pode usar essa função com variáveis ​​ou expressões, tornando-a flexível e adaptável às suas necessidades:

```Elm
nome <- "pedro"
String.capitalize_nome
-- Resultado: "Pedro"
```

## Mergulho Profundo

A função `String.capitalize` é útil em situações simples, mas e se você precisar capitalizar cada palavra de uma string? Nesse caso, podemos utilizar a biblioteca `elm-community/string-extra`, que fornece a função `String.Extra.capitalizeWords`. Essa função é um pouco mais complexa, pois também lida com a capitalização de palavras compostas e de letras acentuadas ou especiais.

Além disso, é importante lembrar que a capitalização varia de acordo com o idioma. Em inglês, por exemplo, nomes próprios e títulos são geralmente capitalizados, enquanto em português nomes próprios de pessoas e cidades não têm suas letras maiúsculas. Portanto, é sempre importante considerar a língua em que o seu aplicativo está sendo desenvolvido.

## Veja Também

- Documentação oficial do Elm sobre a função `String.capitalize`: https://package.elm-lang.org/packages/elm/core/latest/String#capitalize
- Biblioteca `elm-community/string-extra`: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- Guia de estilo de código Elm: https://github.com/NoRedInk/style-guide/blob/master/elm.md#be-consistent