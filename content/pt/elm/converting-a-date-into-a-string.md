---
title:                "Elm: Transformando uma data em uma string"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string pode ser útil em várias situações, como exibir uma data em um formato específico ou armazená-la em um banco de dados. Ao converter a data para uma string, você pode personalizar sua apresentação de acordo com suas necessidades.

## Como fazer

Para converter uma data em uma string em Elm, usamos a função `Date.toString` e especificamos o formato de saída desejado. Por exemplo, se quisermos exibir a data atual no formato "DD/MM/AAAA", podemos usar o seguinte código:

```
import Date exposing (toString)

currentDate : Date.Date
currentDate = Date.now

formattedDate : String
formattedDate = Date.toString "DD/MM/YYYY" currentDate

```

O resultado seria "26/06/2021", dependendo da data atual. O formato pode ser personalizado de acordo com suas preferências, utilizando as letras especificadas pela documentação do Elm.

## Profundidade

Converter uma data em uma string pode não parecer complicado, mas existem algumas coisas importantes a serem consideradas. Por exemplo, o comportamento da função `Date.toString` pode variar dependendo da sua versão do Elm. É sempre importante verificar a documentação para garantir que o formato que você está utilizando seja compatível com a sua versão do Elm.

Além disso, é importante lembrar que a função `Date.toString` não pode converter datas vazias. Se você estiver trabalhando com datas que podem ser nulas, é importante tratar esse cenário antes de converter a data em uma string.

## Veja também

- Documentação oficial do Elm sobre `Date.toString`: <https://package.elm-lang.org/packages/elm/time/latest/Date#toString>
- Artigo sobre formatação de datas em Elm: <https://korban.net/posts/elm/2018-08-04-elm-time-string-formatting/>
- Fórum do Elm sobre dúvidas relacionadas a conversão de datas em strings: <https://discourse.elm-lang.org/t/date-tostring-format-tz/3507>