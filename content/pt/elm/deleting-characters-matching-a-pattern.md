---
title:                "Apagando caracteres que correspondem a um padrão."
html_title:           "Elm: Apagando caracteres que correspondem a um padrão."
simple_title:         "Apagando caracteres que correspondem a um padrão."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao lidar com grandes quantidades de texto, pode ser útil excluir caracteres que correspondam a um determinado padrão. Isso pode ser útil, por exemplo, na limpeza de dados para análise posterior.

## Como fazer

Para excluir caracteres correspondentes a um padrão, iremos utilizar a função `String.filter` do Elm. Esta função recebe uma função de filtro como argumento e retorna uma string com os caracteres que correspondem ao padrão.

Um exemplo simples é excluir todos os caracteres numéricos de uma string:

```Elm
import String exposing (filter)

stringSemNumeros : String -> String
stringSemNumeros texto =
    filter (\char -> not (Char.isDigit char)) texto
```
Exemplo de saída:
```
> stringSemNumeros "Olá2 Mundo!"
"Olá Mundo!"
```

Em nosso exemplo, utilizamos a função `Char.isDigit` para verificar se o caractere é um número e, em seguida, invertemos o resultado com o uso de `not` para garantir que apenas caracteres que não correspondem ao padrão sejam mantidos.

## Mergulho Profundo

A função `String.filter` é muito versátil e pode ser utilizada de diversas maneiras. Ela também pode ser combinada com outras funções de string para criar filtros mais complexos.

Por exemplo, podemos criar uma função para remover todos os caracteres não alfanuméricos de uma string:

```Elm
import String exposing (filter, toLower)

stringSemNaoAlfanumerico : String -> String
stringSemNaoAlfanumerico texto =
    filter (\char -> Char.isAlpha char || Char.isDigit char || char == " ") (toLower texto)
```

Neste exemplo, utilizamos as funções `Char.isAlpha` e `Char.isDigit` para verificar se o caractere é alfabético ou numérico, e também adicionamos a condição de que o espaço em branco deve ser mantido. Além disso, utilizamos a função `toLower` para converter todos os caracteres em minúsculos antes de aplicar o filtro.

## Veja também

- Documentação oficial do Elm sobre a função `String.filter`: https://package.elm-lang.org/packages/elm/core/latest/String#filter
- Outras funções úteis do módulo `String`: https://package.elm-lang.org/packages/elm/core/latest/String