---
title:                "Capitalizando uma string"
html_title:           "Elm: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Capitalização de strings se refere a transformar a primeira letra de uma string em maiúscula. Programadores utilizam estes métodos para formatar saídas ou ordenar alfabeticamente, dependendo do caso.

## Como fazer:

No Elm, para capitalizar uma string, vamos usar a função `toUpper` do módulo `String` para transformar a primeira letra em maiúscula e a função `toLowerCase` para deixar o restante da string em minúsculas. Aqui está o exemplo:

```Elm
import String

capitalize : String -> String
capitalize word =
    case String.uncons word of
        Nothing ->
            ""

        Just ( first, rest ) ->
            String.toUpper (String.fromChar first) ++ String.toLower rest
```

Essa função 'capitalize' recebe uma string e devolve a mesma string, mas com a primeira letra em maiúscula. Se a string for vazia, ela devolve uma string vazia.

## Aprofundamento

A capitalização de strings se originou da necessidade de manipular texto em software para criar uma saída formatada ou para ordenação alfabética. No caso do Elm, essa funcionalidade não é incorporada diretamente na linguagem, mas pode ser conseguida combinando outras funções de string.

A alternativa à função 'capitalize' que construímos seria usar uma biblioteca externa que provavelmente teria uma implementação semelhante.

Além disso, um detalhe de implementação a notar é que as funções `toUpper` e `toLower` não suportam todos os idiomas. Por exemplo, eles não funcionarão corretamente com caracteres não latinos.

## Veja também

Para mais informações sobre o módulo String e suas funções, você pode acessar a documentação oficial [aqui](https://package.elm-lang.org/packages/elm/core/latest/String).