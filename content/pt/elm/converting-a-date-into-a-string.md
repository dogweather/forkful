---
title:                "Convertendo uma data em uma string"
html_title:           "Elm: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com datas em sua aplicação Elm, pode ser necessário convertê-las em uma string para exibi-las de forma mais legível para o usuário. Felizmente, a linguagem Elm possui recursos integrados para facilitar essa conversão.

## Como Fazer

Para converter uma data em uma string, podemos usar a função `Format.date` e especificar o formato desejado. Por exemplo, para obter a data atual em formato de texto, podemos usar o seguinte código:

```Elm
currentDate : Date
currentDate = Time.now |> Date.fromPosix

formattedDate : String
formattedDate = Format.date "%d/%m/%Y" currentDate

-- Output: "26/09/2021"
```

Podemos personalizar o formato da string de acordo com nossas necessidades, usando as letras de formatação específicas. No exemplo acima, `%d` representa o dia, `%m` o mês e `%Y` o ano. Um recurso útil é que podemos adicionar caracteres entre as letras de formatação, como barras ou hifens, para formatar a string de maneira mais legível.

Também podemos utilizar a função `Date.dayOfWeek` para obter o dia da semana correspondente a uma data específica e exibi-la em nossa string formatada. Por exemplo:

```Elm
dayOfWeek : String
dayOfWeek = Date.dayOfWeek currentDate

-- Output: "Sunday"
```

Observe que, para usar essas funções, é necessário importar os módulos `Date` e` Format` em nosso código Elm.

## Deep Dive

Além dos exemplos mencionados acima, a função `Format.date` possui muitas outras letras de formatação que podem ser utilizadas de forma criativa. Por exemplo, `%b` pode ser usado para obter o mês abreviado (por exemplo, "Jan"), `%a` para obter o dia da semana abreviado (por exemplo, "Sun") e `%m` para exibir o mês em forma numérica (por exemplo, "01" para Janeiro).

Também podemos usar a função `Format.dateWithOptions` para especificar configurações adicionais, como o formato de data local e a utilização de 24 horas em vez de 12 horas.

Além disso, se quisermos exibir uma data no formato ISO padrão, podemos usar a função `Date.toIsoString`. Essa função também pode ser útil para comparar diferentes datas, já que ela retorna uma string formatada que pode ser facilmente comparada.

## Veja Também

* [Documentação Oficial do Elm - Date](https://package.elm-lang.org/packages/elm/time/latest/Time-Date)
* [Guia de Formatação de Datas no Elm](https://pianocomposer321.medium.com/elm-formatting-dates-in-elm-2c65471d630f)
* [Fórum da Comunidade Elm - Conversão de datas para strings](https://discourse.elm-lang.org/t/converting-dates-to-strings/1902)