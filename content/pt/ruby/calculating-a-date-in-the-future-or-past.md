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

## O Que & Por Que?

Calcular uma data no futuro ou no passado é quando alteramos uma data atual para obter uma nova data, avançando ou retrocedendo certo número de dias, meses ou anos. Programadores fazem isso para resolver problemas que incluem marcar eventos, agendar tarefas e calcular prazos.

## Como fazer:

O Ruby é equipado com uma classe `Date` prónica para manipulação de datas. 

Aqui está como você pode calcular uma data futura ou passada:

```Ruby
require 'date'

# Calculando uma data futura
date = Date.new(2021, 9, 1)
future_date = date + 30
puts future_date
#=> 2021-10-01

# Calculando uma data passada
date = Date.new(2021, 9, 1)
past_date = date - 30
puts past_date
#=> 2021-08-02
```

## Mergulhando Fundo:

As funções de data têm uma longa história na programação, remontando aos primeiros dias da linguagem COBOL na década de 1960. Em Ruby, a manipulação de datas foi facilitada com a classe `Date`, que é imutável - uma vez que uma instância é criada, ela não pode ser alterada, a implementação é thread-safe.

Existem bibliotecas alternativas como `ActiveSupport::TimeWithZone` para lidar com zonas horárias, que não estão disponíveis na classe `Date`.

Quando adicionar ou subtrair dias, meses ou anos, é importante lembrar que o Ruby conta meses com 30 dias, o que pode resultar em comportamento inesperado em meses com mais ou menos de 30 dias.

## Veja Também:

- [Ruby API Dock - Date](https://apidock.com/ruby/Date)
- [Ruby Guides - Ruby DateTime](https://www.rubyguides.com/2015/12/ruby-time-date-datetime/)
- [Ruby Documentation - Calculations for Date](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)