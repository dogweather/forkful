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

## O que e Por que?
Comparar duas datas é uma tarefa comum em programação, que envolve verificar se uma data é anterior, posterior ou igual a outra. Programadores realizam essa tarefa para garantir a integridade dos dados e tomar decisões baseadas nas datas, como por exemplo, exibir conteúdo somente a partir de uma determinada data.

## Como fazer:
Usando o Gleam, podemos comparar dois valores do tipo Datetime usando o operador ">", "<" ou "==". Confira o exemplo abaixo:
```Gleam
let hoje = Datetime.utc_now()
let amanha = Datetime.utc_add_days(hoje, 1)

if (amanha > hoje) {
  io.format("Amanhã é depois de hoje \n")
}
```
O código acima verifica se a data de amanhã é posterior à data de hoje e imprime uma mensagem.

## Profundidade:
Comparar datas é essencial em muitas aplicações, seja para validar a entrada de dados ou para calcular diferenças de tempo. O Gleam oferece a função ```Datetime.diff_in_days```, que calcula o número de dias entre duas datas. Além disso, é possível utilizar bibliotecas como o "timex" e "calendar" para ter mais opções na manipulação de datas.

## Veja Também:
- [Documentação do Gleam sobre o tipo Datetime](https://gleam.run/examples/dates)
- [Documentação do Gleam para a função diff_in_days](https://hexdocs.pm/gleam_stdlib/Datetime.html#diff_in_days/2)
- [GitHub da biblioteca timex](https://github.com/bitwalker/timex)
- [GitHub da biblioteca calendar](https://github.com/lau/calendar)