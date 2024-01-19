---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Parsear uma data de uma string envolve a conversão de uma data em formato de texto para um objeto de data. Programadores fazem isso para poderem manipular e utilizar datas de maneira mais eficiente em seus códigos.

## Como fazer:

Vamos usar a biblioteca `elm/time` que facilita trabalhar com datas. Aqui está como você parsearia uma data de uma string em Elm:

```Elm
import Time exposing (..)
import Time.Zone as Zone

stringParaData : String -> Maybe Time.Posix
stringParaData dataString =
    Time.fromIsoString dataString

-- Exemplo de uso
isoString = "1991-05-21T00:00:00Z"

case stringParaData isoString of
    Just data ->
        -- Faz algo com a data
    Nothing ->
        -- Handle do caso onde a string não é uma data válida
```

## Mergulho Profundo

No contexto histórico, as datas e horas em Elm são representadas como um valor `Posix`, que é um número representando os milissegundos desde a época Unix (00:00:00 UTC em 1 de janeiro de 1970). Essa abordagem é um padrão comum em muitas linguagens de programação.

Como alternativas, dependendo do seu projeto, você pode querer usar bibliotecas externas como `elm-date-extra`, que oferecem funções adicionais de parseamento.

Em relação aos detalhes de implementação, é importante notar que a função `fromIsoString` só aceita strings na norma ISO 8601.

Além disso, em Elm, funções que podem falhar geralmente retornam um `Maybe` porque Elm não possui exceções em tempo de execução. Portanto, nosso exemplo retorna `Maybe Time.Posix`.

## Veja também 

Para mais informações sobre `elm/time`, veja a [documentação oficial](https://package.elm-lang.org/packages/elm/time/latest/)

Para a norma ISO 8601, veja a descrição no [Wikipedia](https://pt.wikipedia.org/wiki/ISO_8601).

Para saber mais sobre manipulação de datas com Elm, você pode conferir esse [artigo detalhado](https://elmprogramming.com/datetime.html).