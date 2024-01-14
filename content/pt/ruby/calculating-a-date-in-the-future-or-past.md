---
title:                "Ruby: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que
Calculando datas passadas e futuras é uma habilidade útil para qualquer programador. Pode ser usada para construir calendários, agendar tarefas e até mesmo prever eventos futuros.

## Como Fazer
Para calcular uma data passada ou futura em Ruby, você pode usar a classe Date do Ruby. Primeiro, importe a classe usando `require 'date'` e, em seguida, inicialize a data atual usando `Date.today`. Em seguida, use os métodos `+=` ou `-=` para adicionar ou subtrair dias, meses ou anos da data atual.

Exemplo de cálculo de 2 dias no futuro:

```Ruby
require 'date'
data_atual = Date.today
data_futura = data_atual + 2
puts data_futura
```

Saída: 2021-11-04

Exemplo de cálculo de 6 meses atrás:

```Ruby
require 'date'
data_atual = Date.today
data_passada = data_atual - 6
puts data_passada
```

Saída: 2021-05-06

## Mergulho Profundo
Além dos métodos `+=` e `-=` mostrados acima, a classe Date do Ruby também possui outros métodos úteis para calcular datas passadas e futuras. Alguns exemplos incluem `next_day`, `prev_day`, `next_month` e `prev_month`, que permitem que você navegue pelas datas de maneira mais específica.

Além disso, é possível realizar operações matemáticas com as datas, como calcular o número de dias entre duas datas ou determinar a data mais recente entre duas datas.

## Veja Também
- [Documentação oficial da classe Date do Ruby](https://docs.ruby-lang.org/en/2.6.0/Date.html)
- [Tutorial: Como calcular datas passadas e futuras em Ruby](https://www.rubyguides.com/2016/10/calculating-dates-ruby/)