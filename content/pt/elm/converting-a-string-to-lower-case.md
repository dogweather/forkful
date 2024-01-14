---
title:                "Elm: Convertendo uma string para minúsculas"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por Que
Por que você deve se preocupar em converter uma string para caixa baixa (lower case)? Isso é importante porque muitas vezes precisamos comparar strings de texto, ignorando a diferença entre letras maiúsculas e minúsculas. Além disso, ao exibir informações para o usuário, é mais esteticamente agradável e consistente usar apenas caixa baixa.

## Como Fazer
Para converter uma string para caixa baixa no Elm, podemos usar a função `String.toLower` que está incluída no módulo String. Aqui está um exemplo de como usá-la:

```Elm
nomeCompleto = "JOÃO DA SILVA"

nomeEmMinusculo = String.toLower nomeCompleto

-- nomeEmMinusculo será "joão da silva"
```

Como podemos ver, a função `toLower` transforma todas as letras maiúsculas da string em letras minúsculas. O resultado é atribuído a uma nova variável, mas também podemos simplesmente usar a função dentro de uma expressão.

## Aprofundando-se
Para entender melhor como a função `toLower` funciona, podemos olhar para a sua implementação no módulo String. Lá, encontramos o seguinte código:

```Elm
toLower : String -> String
toLower string =
    toLowerHelp string List.reverse

toLowerHelp : String -> List Char -> String
toLowerHelp string acc =
    case String.uncons string of
        Nothing ->
            acc |> List.reverse |> String.fromList

        Just ( first, rest ) ->
            let
                replacement =
                    Char.toLower first
            in
            toLowerHelp rest ( replacement :: acc )
```

Essa implementação usa a recursão para percorrer cada caractere da string original e transformá-lo para caixa baixa. Também é interessante notar que a função `toLower` retorna uma nova string, em vez de modificar a original. Isso se deve ao fato de que, em Elm, as strings são imutáveis (não podem ser alteradas).

## Veja Também
- Documentação do módulo String (em inglês): https://package.elm-lang.org/packages/elm/core/latest/String
- Função `String.toLower` no Ellie (editor de Elm online): https://ellie-app.com/new
- Tutorial básico de Elm (em português): https://medium.com/@kazuyuki_rokuta/como-programar-em-elm-instala%C3%A7%C3%A3o-de80ad5039f3