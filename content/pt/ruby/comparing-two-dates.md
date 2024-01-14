---
title:                "Ruby: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar duas datas é uma tarefa comum na programação, especialmente em projetos que envolvem gerenciamento de tempo e cronogramas. Quando lidamos com datas, é importante saber como compará-las para garantir que nosso código esteja funcionando corretamente e fornecendo resultados precisos.

## Como comparar duas datas em Ruby

Comparar duas datas em Ruby é bastante simples. Podemos usar o método `compare` da classe `Date`, que retorna -1, 0 ou 1, dependendo se a primeira data é anterior, igual ou posterior à segunda data, respectivamente.

```Ruby
require 'date'

date_1 = Date.new(2021, 10, 1)
date_2 = Date.new(2021, 9, 1)

p date_1.compare(date_2)
# output: 1
```

Também podemos utilizar os operadores `>`, `<` e `==` para comparar datas e obter um resultado semelhante.

```Ruby
p date_1 > date_2
# output: true

p date_1 == date_2
# output: false
```

É importante lembrar que, ao comparar datas, devemos ter certeza de que estamos trabalhando com objetos da classe `Date` ou `DateTime`, pois caso contrário, podemos obter resultados inesperados.

## Mergulho Profundo

Quando comparamos datas, é importante entender que não estamos comparando apenas o dia, mês e ano, mas também o horário. Por exemplo, se tivermos as datas 2021-10-01 12:00:00 e 2021-10-01 15:00:00, elas serão consideradas diferentes se usarmos o operador `==` ou o método `compare` da classe `Date`, pois o horário é diferente, mesmo que o dia, mês e ano sejam os mesmos.

Além disso, devemos ter cuidado ao comparar datas em diferentes fusos horários, pois isso também pode afetar os resultados.

## Veja também

- [Documentação oficial da classe Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-c-compare)
- [Guia de Data e Hora em Ruby](https://www.rubyguides.com/2015/01/ruby-date-time/)
- [Comparando datas no Ruby por Ignat Sarna](https://igor.io/2012/12/04/comparing-dates-ruby.html)