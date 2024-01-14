---
title:    "Elm: Utilizando expressões regulares"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar Expressões Regulares em Elm?

As expressões regulares são uma ferramenta poderosa para manipulação de strings em Elm. Elas permitem encontrar e manipular padrões de texto de forma eficiente, economizando tempo e esforço do programador. Além disso, o uso de expressões regulares pode deixar o código mais organizado e legível.

## Como utilizar Expressões Regulares em Elm

Em Elm, podemos utilizar expressões regulares através do módulo Regex. Para começar, é preciso importar este módulo no arquivo Elm:

```Elm
import Regex exposing (Regex)
```

Uma vez que o módulo está importado, podemos criar uma expressão regular utilizando a função `regex` e passando um padrão entre aspas:

```Elm
regex "padrão"
```

Para encontrar um padrão em uma string, utilizamos a função `find` e passamos a expressão regular e a string como parâmetros:

```Elm
Regex.find (regex "padrão") "exemplo de string"
```

Este código irá retornar um valor do tipo `Maybe Regex.Result`, que contém informações sobre o resultado da busca. Para obter o resultado em si, podemos utilizar a função `Maybe.map` e passar uma função que irá manipular o resultado, caso ele exista:

```Elm
Maybe.map (\result -> Regex.Result.matchedText result) (Regex.find (regex "padrão") "exemplo de string")
```

Este código irá retornar uma string contendo o texto encontrado pelo padrão na string original.

## Explorando mais a fundo as Expressões Regulares em Elm

O uso de expressões regulares em Elm vai além de simples comparação de padrões. Com o módulo Regex, podemos encontrar e substituir padrões em strings, extrair grupos de uma expressão e até mesmo validar a formatação de uma string.

Além disso, é possível criar expressões regulares mais complexas utilizando símbolos e metacaracteres. É importante ter conhecimento sobre esses recursos para utilizar expressões regulares de forma eficiente em Elm.

## Veja também

- Documentação oficial do módulo Regex em Elm: https://package.elm-lang.org/packages/elm/regex/latest/
- Tutorial sobre expressões regulares em Elm: https://dev.to/soenke/material-design-for-angular-6-565j
- Repositório com exemplos de uso de expressões regulares em Elm: https://github.com/JoelQ/elm-regex-examples