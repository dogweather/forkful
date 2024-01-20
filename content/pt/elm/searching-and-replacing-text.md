---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Artigo sobre a programação Elm: Pesquisar e substituir texto

## O quê & Por quê?
Pesquisar e substituir texto significa localizar uma cadeia de caracteres específica em uma quantidade significativa de texto e alterá-la por outra. Os programadores fazem isso para manipular dados de texto, corrigir erros ou modificar funcionalidades sem reinventar a roda.

## Como fazer:

Aqui está um exemplo básico de como procurar e substituir texto em Elm. Para isso, vamos usar a função built-in `String.replace`.

```Elm
import String

main =
    let
        originalText = "Amo programar em Elm"
        searchText = "Elm"
        replaceWithText = "JavaScript"
        newText = String.replace searchText replaceWithText originalText
    in
    Html.text newText
```

No exemplo acima, "Elm" será substituído por "JavaScript". Portanto, a saída da função será "Amo programar em JavaScript".

## Deep Dive

Pesquisar e substituir texto é uma funcionalidade básica que tem sido parte das linguagens de programação desde os seus primórdios. Normalmente, essas operações seriam complexas, mas as linguagens modernas, como Elm, tornaram essa tarefa simples.

Como alternativa, a manipulação direta de Strings pode ser usada para pesquisar e substituir texto, embora seja mais complexa e propensa a erros. Além disso, as vezes, pode ser mais adequado o uso de expressões regulares para pesquisas mais complexas.

A implementação da função `String.replace` em Elm passa por analisar o texto original e reconstruí-la com a substituição, quando a sequência de pesquisa é encontrada.

## Ver também

Para mais detalhes, pode referir ao seguinte:

1. Documentação Elm sobre manipulação de texto - [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
2. Um artigo detalhado sobre expressões regulares em Elm - [Elm Regular Expressions](https://elmprogramming.com/regular-expression.html)
3. Uma explicação clara e concisa do conceito de busca e troca em [Wikipedia](https://en.wikipedia.org/wiki/Find_and_replace)