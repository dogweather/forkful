---
title:                "Elm: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter datas em strings é uma tarefa comum no desenvolvimento de aplicativos, especialmente quando se lida com exibição de datas para os usuários. Em Elm, essa conversão é uma função simples, mas pode ser muito útil para melhorar a experiência do usuário.

## Como fazer

Para converter uma data em uma string, podemos usar a função `Date.toString` do pacote `elm/time`.

```
elm install elm/time
```

Em seguida, podemos utilizar a função para converter uma data em uma string no formato desejado. Por exemplo, se tivermos uma data no formato "YYYY-MM-DD", podemos converter para o formato brasileiro "DD/MM/YYYY" da seguinte forma:

```
import Date exposing (Date)
import Time exposing (Date, toYear, toMonth, toDay)

dateToString : Date -> String
dateToString date =
  let
    day = toDay date
    month = String.padLeft 2 '0' (toMonth date |> String.fromInt)
    year = toYear date
  in
    day ++ "/" ++ month ++ "/" ++ year
```

A função `padLeft` é usada para adicionar um zero à esquerda caso o mês tenha apenas um dígito. Caso o formato desejado seja diferente, basta alterar a formatação do retorno.

## Profundidade

Ao olhar para a fonte da função `Date.toString`, podemos ver que a conversão é feita de forma bastante simples, utilizando as funções `toYear`, `toMonth` e `toDay`.

Essas funções, por sua vez, utilizam uma série de cálculos matemáticos para obter o ano, mês e dia da data. Além disso, há também uma série de funções para lidar com fusos horários, que podem ser úteis em casos específicos.

É importante notar que a formatação padrão da função `Date.toString` segue o padrão ISO 8601, que pode não ser o formato preferido para todos os casos. Por isso, é importante entender como funciona a conversão e adaptar de acordo com a necessidade.

## Veja também

- [Documentação oficial do pacote `elm/time`](https://package.elm-lang.org/packages/elm/time/latest/)
- [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)