---
title:                "Exclusão de caracteres que correspondam a um padrão"
html_title:           "Elm: Exclusão de caracteres que correspondam a um padrão"
simple_title:         "Exclusão de caracteres que correspondam a um padrão"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

O que & Por quê?
Deletar caracteres que correspondam a um padrão é uma técnica usada pelos programadores para remover partes indesejadas de uma String. Isso pode ser útil para limpar dados ou filtrar informações em uma aplicação.

Como fazer:
Para deletar caracteres que correspondam a um padrão em Elm, podemos usar a função `String.filter` combinada com a função `String.startsWith` (para verificar se uma String começa com um determinado padrão) ou `String.regex` (para usar expressões regulares). Aqui está um exemplo de como usar ambas as funções:

```
Elm String.filter (\char -> not (String.startsWith "a" char)) "apple"
```

Este código retornará "pple" após filtrar os caracteres que começam com "a" na String "apple". Você também pode usar `String.regex` para filtrar usando uma expressão regular, como mostrar o exemplo a seguir:

```
Elm String.filter (String.regex "ab+") "apple"
```

Este código irá retornar "e" após filtrar todos os caracteres que correspondem à expressão regular "ab+" na String "apple".

Aprofundando:
A técnica de deletar caracteres que correspondam a um padrão tem sido usada há muito tempo pelos programadores em várias linguagens de programação. Outras linguagens, como JavaScript e Python, também têm funções semelhantes que permitem aos programadores filtrar Strings com base em um padrão.

Uma das principais vantagens de usar `String.filter` em Elm é a sua tipagem estática. Isso significa que o compilador do Elm irá detectar erros de tipo em tempo de compilação, o que torna o código mais seguro e reduz a ocorrência de bugs durante o processo de desenvolvimento.

Veja também:
- Funções `String.filter` e `String.startsWith`: [Documentação do Elm](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- Função `String.regex`: [Documentação do Elm](https://package.elm-lang.org/packages/elm/regex/latest/Regex#find)
- Tutorial sobre como usar `String.filter`: [Elm Tutorials](https://elmprogramming.com/tutorial/strings.html#string-filter)