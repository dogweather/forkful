---
title:                "Comparando duas datas"
html_title:           "Ruby: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como comparar duas datas em um programa Ruby? Essa é uma habilidade importante para lidar com dados de tempo e agendar tarefas em seu código. Neste artigo, vamos explorar como realizar essa tarefa de forma eficiente e efetiva.

## Como fazer

Para comparar duas datas em Ruby, podemos usar o método `Date#compare`, que retorna um número indicando se a primeira data é anterior, igual ou posterior à segunda data. Veja um exemplo de código abaixo:

```Ruby
require 'date'

data1 = Date.parse('2021-10-01')
data2 = Date.parse('2021-09-01')

puts data1.compare(data2) # resultado: 1 (data1 é depois de data2)
```

Podemos também usar o operador de comparação `>` para verificar se uma data é posterior à outra. Observe o exemplo abaixo:

```Ruby
require 'date'

data1 = Date.parse('2021-10-01')
data2 = Date.parse('2021-09-01')

puts data1 > data2 # resultado: true
```

É importante lembrar que o método `Date#compare` considera apenas a parte da data, ignorando informações de hora e fuso horário.

## Deep Dive

Em Ruby, as datas são armazenadas como objetos do tipo `Date`, que são baseados no calendário gregoriano. Cada data possui atributos como dia, mês e ano, e também pode conter informações sobre o fuso horário e a fração de segundos.

Ao comparar duas datas, é importante levar em conta a precisão das informações. Se as datas possuírem informações de hora e fuso horário diferentes, por exemplo, elas podem ser consideradas iguais como objetos `Date`, mas terão resultados diferentes ao serem comparadas usando o método `Date#compare`.

Além disso, é importante lembrar que o método `Date#compare` retorna um número que pode ser interpretado como -1, 0 ou 1, indicando se a primeira data é anterior, igual ou posterior à segunda data, respectivamente.

## Veja também

- [Documentação do Ruby sobre o método `Date#compare`](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-compare)
- [Tutorial sobre manipulação de datas em Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [Exemplos de uso do operador de comparação `>` em Ruby](https://www.geeksforgeeks.org/ruby-operators/#Comparison)