---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é e Por que?

Parsing HTML é o ato de converter código HTML em uma representação estruturada e manipulável, geralmente uma árvore, chamada de DOM (document object model). Programadores fazem isso para obter, alterar ou adicionar conteúdo a páginas da web de maneira programática.

## Como Fazer:

Aqui está um exemplo básico de como analisar um HTML em Elm por meio da biblioteca elm/html:

```Elm
import Html.Parser
import Html.Parser.Util

códHtml : String
códHtml = "<p>Olá mundo!</p>"

parseHtml : String -> Maybe (List Html.Parser.Node)
parseHtml = 
  Html.Parser.run (Html.Parser.Util.spaces >> Html.Parser.Util.ignoreComments >> Html.Parser.nodes)

main : Html a
main =
  text (case parseHtml códHtml of
    Just nodes ->
      toString nodes

    Nothing ->
      "Erro na análise do HTML!")
```

Este programa irá imprimir: Just [Element "p" [] [Text "Olá mundo!"]].

## Deep Dive:

A análise de HTML tem uma longa história, tendo sido uma parte vital da web desde os seus primeiros dias. Originalmente, a análise de HTML costumava ser feita à mão com ajuda de regex. No entanto, com a evolução das páginas da web e a necessidade de analisar HTML mais complexo, essa abordagem caiu em desuso.

Hoje, utilizamos parsers de HTML, como o `elm/html`, para simplificar esse processo. Eles são mais robustos, conseguindo lidar com HTML mal formado, e fornecem uma interface fácil de usar.

Existem alternativas ao `elm/html`, como o `hecrj/html-parser`, mas cada um tem suas próprias vantagens e desvantagens. Por exemplo, `hecrj/html-parser` dá um controle mais granular sobre a análise, mas tem um desempenho levemente inferior.

## Ver Também:

1. [Documentação do elm/html](https://package.elm-lang.org/packages/elm/html/latest/)
2. [Análise de HTML com o hecrj/html-parser](https://package.elm-lang.org/packages/hecrj/html-parser/latest/)
3. [Um guia mais detalhado sobre análise de HTML em Elm](https://elm-programming.com/parsing-html.html)
4. [Análise de HTML com Regex, uma abordagem histórica](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)