---
title:                "Elm: Analisando o html"
simple_title:         "Analisando o html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Se você é um desenvolvedor web, provavelmente já se deparou com a tarefa de analisar o código HTML de uma página para extrair informações específicas. Talvez você precise fazer isso para extrair dados de um site ou para criar recursos dinâmicos. Independentemente do motivo, a análise de HTML pode ser uma tarefa tediosa e demorada. Felizmente, o Elm oferece uma maneira fácil e segura de fazer isso.

## Como Fazer

A linguagem de programação Elm possui uma biblioteca chamada elm/parser que nos permite criar uma função que analisa o código HTML de forma eficiente e precisa. Veja um exemplo de função que usa a biblioteca elm/parser para analisar uma tag `<h1>` em um documento HTML:

```elm
import Html exposing (..)
import Parser exposing (..)
import String exposing (..)

headerParser : Parser String
headerParser =
    let
        openTag =
            symbol "<h1>"
        closeTag =
            symbol "</h1>"
        header =
            takeUntil closeTag
    in
    succeed header
        |. openTag
        |= header
        |= closeTag

extractHeader : String -> Maybe String
extractHeader html =
    case run headerParser html of
        Err _ ->
            Nothing
        Ok header ->
            Just header

main =
    Html.text (extractHeader "<h1>Hello World</h1>")
```

Neste exemplo, criamos uma função `headerParser` que utiliza funções da biblioteca elm/parser para analisar uma tag `<h1>`. A função `extractHeader` utiliza a função `run` para executar o nosso parser no código HTML e extrair o conteúdo da tag `<h1>`. Por fim, no nosso `main`, utilizamos a função `extractHeader` para exibir o conteúdo da tag no navegador.

## Deep Dive

A biblioteca elm/parser possui diversos parsers já implementados, que podem ser facilmente combinados para analisar qualquer tipo de código HTML. Além disso, a linguagem Elm possui um sistema de tipos muito forte, que ajuda a evitar erros durante a análise do código. Isso significa que podemos confiar nos resultados obtidos pelo nosso parser.

Outra vantagem de utilizar o elm/parser é que ele possui um desempenho muito bom, graças às suas otimizações internas baseadas em algoritmos de análise léxica e gramatical.

## Veja Também

- Documentação da biblioteca elm/parser: https://package.elm-lang.org/packages/elm/parser/latest/
- Tutorial de análise de HTML com o elm/parser: https://dev.to/joelmansilla/parsing-html-trees-with-elm--15ah
- Exemplo de projeto em Elm utilizando o elm/parser para análise de HTML: https://github.com/mdgriffith/elm-markup