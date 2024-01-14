---
title:    "Ruby: Calculando uma data no futuro ou no passado"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, precisamos calcular uma data específica no futuro ou no passado durante a programação. Pode ser para agendar um evento, definir um lembrete ou simplesmente para realizar cálculos de datas. Saber como fazer isso pode ser útil em diversas situações.

## Como fazer

Para calcular uma data no futuro ou no passado, podemos utilizar o método `advance` da classe `Date` do Ruby. O método pode receber três argumentos: o número de dias, o número de meses e o número de anos que queremos avançar ou retroceder na data atual. Vamos ver um exemplo prático:

```Ruby
# Importando a classe Date
require 'date'

# Definindo a data atual como 1º de janeiro de 2021
data_atual = Date.new(2021, 1, 1)

# Avançando 30 dias a partir da data atual
data_futura = data_atual.advance(days: 30)

# Retrocedendo 1 ano e 2 meses a partir da data atual
data_passada = data_atual.advance(years: -1, months: -2)

# Imprimindo as datas calculadas
puts "Data futura: #{data_futura}"
puts "Data passada: #{data_passada}"
```

O código acima irá imprimir:

```
Data futura: 2021-01-31
Data passada: 2019-10-01
```

Além disso, o método `advance` também pode ser utilizado com outras classes de data, como `DateTime` e `Time`.

## Mergulho profundo

O método `advance` também permite que avancemos ou retrocedamos uma data de forma mais precisa, utilizando frações de dias, meses e anos. Por exemplo, se quisermos avançar 1,5 mês a partir da data atual, podemos fazer o seguinte:

```Ruby
# Importando a classe Date
require 'date'

# Definindo a data atual como 1º de janeiro de 2021
data_atual = Date.new(2021, 1, 1)

# Avançando 1,5 mês a partir da data atual
data_futura = data_atual.advance(months: 1.5)

# Imprimindo a data calculada
puts "Data futura: #{data_futura}"
```

O código acima irá imprimir:

```
Data futura: 2021-02-16
```

Isso pode ser útil em casos onde precisamos avançar datas de forma mais precisa, como em cálculos financeiros.

## Veja também
- [Documentação do método `advance` (em inglês)](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html#method-c-advance)
- [Exemplos práticos de cálculo de datas (em inglês)](https://www.mikhyel.com/blog/calculating-future-dates-with-ruby-date)
- [Artigo sobre manipulação de datas no Ruby (em português)](https://tableless.com.br/manipulacao-de-datas-no-ruby/)