---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Gleam: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou no passado?

Às vezes precisamos saber qual será a data em um determinado número de dias, semanas ou meses a partir de agora, ou mesmo em um passado distante. Por exemplo, para planejar uma viagem ou encontrar uma data específica em um calendário.

## Como Fazer

Para calcular uma data no futuro ou no passado usando Gleam, precisamos usar a função `DateTime.add_days()` e especificar a quantidade de dias que desejamos adicionar ou subtrair da data atual. Veja um exemplo de código abaixo:

```Gleam
let data_atual = DateTime.now()
let data_no_futuro = DateTime.add_days(data_atual, 10)
let data_no_passado = DateTime.add_days(data_atual, -5)
```

A saída desse código seria:

- `data_no_futuro`: 10 dias a partir de agora
- `data_no_passado`: 5 dias antes da data atual.

Também podemos adicionar ou subtrair semanas ou meses usando as funções `DateTime.add_weeks()` e `DateTime.add_months()`, respectivamente. Veja um exemplo:

```Gleam
let data_atual = DateTime.now()
let data_daqui_um_mes = DateTime.add_months(data_atual, 1)
let data_ha_tres_semanas = DateTime.add_weeks(data_atual, -3)
```

A saída seria:

- `data_daqui_um_mes`: um mês a partir de agora
- `data_ha_tres_semanas`: três semanas antes da data atual.

## Mergulho Profundo

A biblioteca `DateTime` do Gleam também permite manipular outras informações de data e hora, como horas, minutos, segundos e milissegundos. É possível criar uma data e hora específica usando a função `DateTime.from_yyyymmdd()`, passando os parâmetros ano, mês e dia como inteiros. Também é possível criar uma data a partir de uma string usando a função `DateTime.from_string()`. A documentação completa e mais exemplos podem ser encontrados [aqui](https://gleam.run/documentation/).

## Veja Também

- [Documentação Gleam](https://gleam.run/documentation/)
- [Strftime: Formatação de data e hora em Gleam](https://harfangk.github.io/2020/03/22/strftime-for-gleam-datetime.html)
- [Calculando diferenças de datas em Gleam](https://dev.to/peterhartree/calculating-differences-between-dates-in-gleam-8pb)