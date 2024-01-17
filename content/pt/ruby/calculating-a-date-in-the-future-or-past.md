---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Ruby: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e por que?

Calcular uma data no futuro ou passado é um processo comum em programação, no qual se determina uma data específica que está a determinada quantidade de tempo antes ou depois de outra data. Os programadores fazem isso para criar funcionalidades de agendamento ou para lidar com datas de uma forma mais dinâmica.

## Como fazer:

```Ruby
# Calcular a data atual
puts Date.today

# Calcular a data daqui a 3 dias
puts Date.today + 3

# Calcular a data de 1 semana atrás
puts Date.today - 7
```

Saída:

```Ruby
# Data atual
2021-06-29

# Data daqui a 3 dias
2021-07-02

# Data de 1 semana atrás
2021-06-22
```

## Mergulho profundo:

Calculadora de datas é uma funcionalidade que tem sido usada há muito tempo em programação, desde os primórdios da computação. Hoje em dia, existem várias bibliotecas e métodos em diferentes linguagens de programação, incluindo Ruby, que facilitam e simplificam o processo de calcular datas no futuro ou passado. Além disso, algumas alternativas incluem utilizar timestamps ou realizar cálculos com intervalos de tempo específicos.

## Veja também:

Links relacionados:

- [Documentação oficial do Ruby sobre DateTime] (https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)
- [Tutorial sobre cálculos de datas em Ruby] (https://www.rubyguides.com/2019/08/ruby-date-time/)
- [Outra biblioteca útil para cálculos de datas em Ruby] (https://github.com/alphagov/pension_date_helper)