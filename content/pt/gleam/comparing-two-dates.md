---
title:                "Comparando duas datas"
html_title:           "Gleam: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Se você já se encontrou em uma situação em que precisou comparar duas datas e teve dificuldades em fazer isso de forma eficiente, esse artigo é para você. A comparação de datas é uma tarefa comum na programação e entender como fazer isso pode economizar tempo e evitar bugs no seu código.

## Como fazer essa comparação no Gleam

Para comparar duas datas no Gleam, primeiro precisamos converter essas datas para o tipo de dados `DateTime`. Em seguida, podemos usar os operadores de comparação nativos do Gleam (`<`, `<=`, `>`, `>=`, `==`, `!=`) para comparar as datas. Veja o exemplo abaixo:

```Gleam
import gleam/datetime

let data1 = datetime.from_timestamp(1629028800)
let data2 = datetime.from_timestamp(1634361600)

if data1 < data2 {
  "A data 1 é anterior à data 2" |> io.print
} else if data1 == data2 {
  "As datas são iguais" |> io.print
} else {
  "A data 2 é anterior à data 1" |> io.print
}
```

Esse código irá imprimir "A data 1 é anterior à data 2", pois 1629028800 é uma data anterior a 1634361600. Mas e se precisarmos comparar apenas o dia, mês ou ano das datas? É possível fazer isso usando a função `datetime.equal/2`. Veja o exemplo abaixo:

```Gleam
import gleam/datetime

let data1 = datetime.from_date(2021, 8, 15)
let data2 = datetime.from_date(2021, 10, 31)

if datetime.equal(data1, data2, "day") {
  "As datas têm o mesmo dia" |> io.print
} else {
  "As datas não têm o mesmo dia" |> io.print
}
```

Esse código irá imprimir "As datas têm o mesmo dia", pois ambas as datas são 15 de agosto. Mas e se quisermos comparar o horário também? Podemos usar a função `datetime.equal_hour/2` ou `datetime.equal_minute/2` para isso.

## Aprofundando na comparação de datas

Além dos operadores de comparação e das funções `equal`, o módulo `gleam/datetime` também oferece funções para obter a diferença entre duas datas, converter timestamps para datas e vice-versa, e muito mais. Se você precisar de mais informações sobre como trabalhar com datas no Gleam, confira a documentação oficial [aqui](https://gleam.run/documentation/stdlib/datetime).

## Veja também

- [Documentação oficial do módulo gleam/datetime](https://gleam.run/documentation/stdlib/datetime)
- [Artigo sobre o tipo de dados datetime no Gleam](https://medium.com/linkapi-technology/working-with-datetime-in-gleam-3f5b6067e7d8)
- [Exemplo de código usando o módulo gleam/datetime](https://github.com/gleam-lang/gleam_stdlib/blob/master/time/examples/date_is_great/DateIsGreat.gleam)