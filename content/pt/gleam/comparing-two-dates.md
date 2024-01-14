---
title:    "Gleam: Comparando duas datas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas em Gleam?

Quando se trabalha com bancos de dados ou com dados temporais em geral, muitas vezes precisamos comparar duas datas para realizar alguma funcionalidade. Em Gleam, isso pode ser feito de forma rápida e eficiente, o que torna a linguagem uma ótima opção para trabalhar com dados temporais. Neste artigo, vamos explorar como fazer isso.

## Como Fazer

Para comparar duas datas em Gleam, precisamos primeiro converter as datas em strings e então utilizar o módulo padrão `Date` da linguagem. A seguir, temos um exemplo de código que compara duas datas e imprime o resultado:

```Gleam
import Date

let data_1 = Date.format({ day: 10, month: Jan, year: 2020 })
let data_2 = Date.format({ day: 15, month: Feb, year: 2020 })

let resultado = Date.compare(data_1, data_2)

sarada.# result: :equal
```

Neste exemplo, utilizamos a função `format` do módulo `Date` para converter as datas em strings no formato "d/m/yyyy", que é o padrão usado em Portuguese. Em seguida, utilizamos a função `compare` para comparar as duas datas. O resultado da comparação é atribuído à variável `resultado`, que pode ser igual a `:equal` se as datas forem iguais, `:preceding` se a primeira data for anterior à segunda, ou `:successor` se a primeira data for posterior à segunda.

## Mergulho Profundo

Além de comparar as datas, o módulo `Date` também oferece outras funcionalidades importantes para trabalhar com datas em Gleam. Por exemplo, podemos usar a função `add_days` para adicionar um determinado número de dias a uma data, ou `is_leap_year` para verificar se um determinado ano é bissexto. Além disso, o módulo também possui funções para trabalhar com horários e fusos horários.

## Veja Também

Para mais informações sobre o módulo `Date` e outras funcionalidades da linguagem Gleam, confira os seguintes recursos:

- [Documentação do módulo Date](https://gleam.run/modules/Date.html)
- [Documentação da linguagem Gleam](https://gleam.run/docs/)