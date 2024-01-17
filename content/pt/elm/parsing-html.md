---
title:                "Análise de HTML"
html_title:           "Elm: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/parsing-html.md"
---

{{< edit_this_page >}}

## O que e por que?

Se você já se perguntou como os navegadores traduzem o código de uma página da web em algo visível e interativo, então você está falando sobre o processo de parsing HTML. Simplificando, o parser HTML é responsável por ler o código fonte de uma página web e transformá-lo em elementos compreensíveis para o navegador. É uma técnica crucial para o desenvolvimento web, pois permite que os programadores criem páginas dinâmicas que são visualmente atraentes e interativas para os usuários.

## Como fazer:

```Elm
import Html.Parser exposing (..)
import Html exposing (text, node)

-- Cria um parser HTML:
myParser : Parser (List (Node a))

-- Lê uma string HTML
doc : String
doc = "<p>Olá mundo!</p>"

-- Faz o parser da string usando a função parse:
html : Result (List (Node a)) Error
html = parse myParser doc

-- Exemplo de saída:
Ok [node "p" [] [text "Olá mundo!"]]

```

## Profundidade

O processo de parsing HTML é algo que vem sendo desenvolvido desde os primeiros dias da internet. No início, os navegadores tinham diferentes interpretações da especificação HTML, o que causava problemas de compatibilidade. Atualmente, há várias alternativas ao parsing HTML, como o uso de linguagens de marcação mais robustas, como XML. No entanto, o parsing HTML continua sendo uma técnica amplamente utilizada devido à sua simplicidade e ampla compatibilidade.

## Veja também:

- [Documentação do pacote HTML.Parser](https://package.elm-lang.org/packages/elm/core/latest/Html-Parser)
- [Especificação HTML do W3C](https://www.w3.org/TR/html/)
- [Outras alternativas ao parsing HTML](https://www.technolush.com/xla/practices/html-compare.html)