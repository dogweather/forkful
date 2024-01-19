---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Expressões regulares são padrões usados para combinar combinações de caracteres em strings. Programadores os usam para buscar, editar e manipular texto de maneira eficiente.

## Como:

Aqui estão alguns exemplos de como usar expressões regulares no Gleam:

```Gleam
import gleam/regex

let example = regex.from_string("[a-z]+")

case example {
    Ok(re) ->
        case regex.match?(re, "ola mundo") {
            True ->
                io.println("Correspondência encontrada!")

            False ->
                io.println("Nenhuma correspondência encontrada.")
        }

    Error(_error) ->
        io.println("Expressão regular inválida.")
}
```

Este script verifica se a string "ola mundo" contém uma ou mais letras minúsculas de 'a' a 'z'.

## Mergulho Profundo

Expressões regulares têm sido uma ferramenta importante para programadores desde os tempos de desenvolvimento do Unix na Bell Labs na década de 1970. Embora possam ser poderosas, também têm desafios e complexidades que levaram ao desenvolvimento de alternativas, como mecanismos de pesquisa de texto completo e analisadores sintáticos.

No Gleam, o módulo `gleam/regex` fornece funcionalidades relacionadas a expressões regulares. Internamente, ele usa a biblioteca Rust regex para fornecer uma implementação segura e rápida para expressões regulares. No entanto, considere que existem limitações na forma como Gleam suporta expressões regulares devido ao seu suporte nativo ao compilador Erlang.

## Veja Também

Caso queira aprofundar o seu conhecimento em expressões regulares no Gleam, consulte os seguintes links:

[Documentação Oficial do Gleam Regex](https://hexdocs.pm/gleam_stdlib/gleam/regex/index.html)

[Recursos de Expressões Regulares em Rust](https://docs.rs/regex/1.3.9/regex/)

[História e Visão Geral de Expressões Regulares](https://en.wikipedia.org/wiki/Regular_expression)