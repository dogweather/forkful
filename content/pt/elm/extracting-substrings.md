---
title:                "Elm: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que utilizar a extração de substrings em Elm?

A extração de substrings é uma técnica muito útil ao trabalhar com strings em Elm. Isso permite que você obtenha pedaços específicos de uma string maior, facilitando o processamento de dados e a criação de algoritmos mais precisos.

## Como fazer a extração de substrings em Elm

Para extrair uma substring em Elm, você precisará da função `String.slice` que recebe três argumentos: a string original, o índice inicial e o índice final. Por exemplo:

```
Elm.String.slice "Hello World!" 0 4
```
Isso retornará a substring "Hello" da string original.

Você também pode utilizar números negativos para o índice final, o que irá contar os caracteres a partir do final da string. Por exemplo:

```
Elm.String.slice "Hello World!" 6 -1
```
Isso retornará a substring "World" da string original.

## Aprofundando na extração de substrings em Elm

Além da função `String.slice`, o pacote `elm/chars` também oferece a função `slice`, que permite extrair substrings com base em caracteres, em vez de índices numéricos. Isso pode ser útil quando você tem uma string com caracteres multibyte, como acentos e emojis.

Você também pode utilizar a função `String.words` para dividir uma string em substrings com base nos espaços em branco. Por exemplo:

```
Elm.String.words "Hello World!"
```
Isso retornará uma lista de strings ["Hello", "World!"].

## Veja também

- Documentação do pacote `elm/chars`: https://package.elm-lang.org/packages/elm/chars/latest/
- Documentação sobre a função `String.words`: https://package.elm-lang.org/packages/elm/core/latest/String#words