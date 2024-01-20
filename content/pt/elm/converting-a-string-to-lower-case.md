---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertendo strings em minúsculas com Elm

## O que é e por quê?
Conversão de strings para minúsculas é o processo de alteração de todas as letras maiúsculas em uma string para letras minúsculas. Programadores fazem isso para normalização de dados e comparações de strings insensíveis a maiúsculas e minúsculas.

## Como fazer:
A biblioteca Elm 'String' oferece uma função chamada 'toLower' para converter uma string em minúsculas. Vamos ver um exemplo:

```Elm
import String

-- Main
main =
    let
        meuTexto = "HELLO, WORLD!"
    in
        String.toLower meuTexto
```

O resultado será "hello, world!".

## Mergulho profundo
A funcionalidade de conversão de string para minúsculas é uma implementação comum em quase todas as linguagens de programação. No contexto histórico, o Elm recebeu a função 'toLower' a partir da versão 0.19.

Quanto às alternativas, você pode implementar sua própria função para converter uma string em minúsculas, mas isso geralmente não é recomendado, pois a função 'toLower' foi otimizada e testada pela comunidade.

A implementação do 'toLower' no Elm, como outras linguagens funcionais, usa um conceito chamado redução ou fold. Ele itera por cada caractere, convertendo para minúsculas e acumulando o resultado.

## Veja também
- Documentação oficial da biblioteca 'String' do Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- Referência Elm para 'toLower': https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Tutoriais de programação Elm em Português: https://elmprogramming.com/