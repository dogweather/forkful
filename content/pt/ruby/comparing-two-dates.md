---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparando duas datas em Ruby

## O que e por quê?

Comparar duas datas significa determinar se uma data é anterior, posterior ou igual a outra. Programadores fazem isso para realizar tarefas como ordenar eventos por data ou calcular a diferença de tempo entre duas datas.

## Como fazer:

No Ruby, você pode comparar duas instâncias de `Date` ou `DateTime` usando operadores de comparação padrão como `>`, `<`, `==`, `>=` e `<=`. 

```Ruby
require 'date'

data1 = Date.new(2022, 6, 15)
data2 = Date.new(2021, 6, 15)

puts data1 > data2  # Retorna true
puts data1 == data2 # Retorna false
puts data1 < data2  # Retorna false
```

Para calcular a diferença entre duas datas, você pode subtrair um objeto `Date` de outro. 

```Ruby
diferenca = data1 - data2
puts diferenca.to_i # Retorna 365
```

## Aprofundando um pouco mais

Historicamente, o comparar e manipular datas sempre foi um desafio para os programadores devido à complexidade do calendário gregoriano e dos fusos horários. Felizmente, Ruby abstrai muitos desses detalhes através das classes `Date` e `DateTime`.

Existem alternativas para a classe `Date` no Ruby, como a gem `time_difference`, que pode fornecer mais recursos e refinamentos, dependendo de suas necessidades específicas.

Ao implementar a comparação de datas, é crucial considerar os fusos horários. No Ruby, `DateTime.now` retorna a data e hora local, enquanto a `Date.today` retorna a data corrente no fuso horário do sistema.

## Veja também

Para saber mais sobre as classes `Date` e `DateTime` em Ruby, visite a documentação oficial:

- [Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [DateTime](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)

Para uma abordagem mais elaborada da manipulação e comparação de datas, considere a gem `time_difference`:

- [time_difference](https://rubygems.org/gems/time_difference/versions/0.5.0)