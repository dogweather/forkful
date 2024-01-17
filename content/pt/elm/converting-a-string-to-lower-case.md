---
title:                "Convertendo uma string para minúsculas"
html_title:           "Elm: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma string para letras minúsculas é um processo simples que muda todas as letras maiúsculas em uma string para minúsculas. Isso pode ser útil em programação quando se precisa comparar strings sem se preocupar com as diferenças entre letras maiúsculas e minúsculas.

## Como fazer:

Para converter uma string para minúsculas em Elm, podemos usar a função "String.toLower", que recebe uma string como argumento e retorna uma nova string com todas as letras em minúsculo.

```Elm
str <- "ELM PROGRAMMING"
lowerStr = String.toLower str
```

O valor de "lowerStr" será "elm programming".

## Aprofundando:

Converter strings para minúsculas pode ser importante para garantir que as comparações de strings sejam feitas de maneira consistente. Por exemplo, se estivermos construindo uma aplicação de login, queremos garantir que não haja diferença nos nomes dos usuários devido a letras maiúsculas ou minúsculas. 

Além disso, em algumas linguagens de programação, como o JavaScript, é comum encontrar casos em que uma string precisa ser transformada em minúscula para executar uma determinada operação, como uma busca em um banco de dados. Essas situações são resolvidas facilmente com a conversão para minúsculo.

Existem outras maneiras de converter strings para minúsculas em Elm, como a utilização da função "map" em conjunto com a função "Char.toLower" para aplicar a conversão em cada caractere individual de uma string. No entanto, a função "String.toLower" já resolve esse problema sozinha, tornando-a a opção mais simples e direta.

## Veja também:

Para mais informações sobre a função "String.toLower", consulte a [documentação oficial do Elm](https://package.elm-lang.org/packages/elm/core/latest/String#toLower). Também é possível encontrar outras funções relacionadas à manipulação de strings na biblioteca padrão do Elm. Experimente explorá-las para melhorar suas habilidades em programação!