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

## Por que

Você pode precisar calcular uma data no futuro ou no passado ao lidar com tarefas e eventos relacionados a tempo no seu código. Isso pode ser feito de forma eficiente utilizando a linguagem de programação Ruby.

## Como fazer

O Ruby possui uma biblioteca padrão chamada `Date` que possui métodos que permitem calcular datas no futuro ou no passado. Primeiro, vamos importar a biblioteca utilizando `require`:

```Ruby
require 'date'

date = Date.new(2020, 5, 1) # definindo uma data inicial
```

Para calcular uma data no futuro, podemos utilizar o método `next_day` e passar como argumento o número de dias que desejamos avançar:

```Ruby
date.next_day(10) # calcula 10 dias no futuro
=> #<Date: 2020-05-11 ((2458981j,0s,0n),+0s,2299161j)>
```

Da mesma forma, para calcular uma data no passado, podemos usar o método `prev_day`:

```Ruby
date.prev_day(5) # calcula 5 dias no passado
=> #<Date: 2020-04-26 ((2458966j,0s,0n),+0s,2299161j)>
```

Se preferir, também podemos utilizar o operador de adição `+` e subtração `-` para calcular datas no futuro e no passado, respectivamente:

```Ruby
date + 15 # calcula 15 dias no futuro
=> #<Date: 2020-05-16 ((2458996j,0s,0n),+0s,2299161j)> 

date - 7 # calcula 7 dias no passado
=> #<Date: 2020-04-24 ((2458964j,0s,0n),+0s,2299161j)>
```

## Mergulho profundo

Além dos métodos mencionados, a biblioteca `Date` também possui outros métodos úteis para trabalhar com datas, como `next_year`, `next_month`, `next_week`, `prev_year`, `prev_month` e `prev_week`. Esses métodos permitem avançar ou retroceder datas em anos, meses e semanas. Você pode consultar a documentação oficial do Ruby para mais detalhes.

## Veja também

- [Documentação oficial do Ruby sobre a biblioteca Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/index.html)
- [Tutorial sobre a biblioteca Date em Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [Exemplos práticos de uso da biblioteca Date em Ruby](https://www.oreilly.com/library/view/ruby-in-a/9780596809502/ch06s07.html)