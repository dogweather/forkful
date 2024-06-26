---
date: 2024-01-20 17:31:59.997062-07:00
description: "Como Fazer: Datas s\xE3o essenciais para a programa\xE7\xE3o desde o\
  \ in\xEDcio dos computadores. No Ruby, a classe `Date` oferece m\xE9todos para manipular\
  \ datas com\u2026"
lastmod: '2024-04-05T21:53:47.468138-06:00'
model: gpt-4-1106-preview
summary: "Datas s\xE3o essenciais para a programa\xE7\xE3o desde o in\xEDcio dos computadores."
title: Calculando uma data no futuro ou passado
weight: 26
---

## Como Fazer:
```Ruby
require 'date'

# Data atual
hoje = Date.today
puts "Hoje: #{hoje}" #=> Hoje: 2023-03-30

# Calcular uma data 10 dias no futuro
futuro = hoje + 10
puts "Daqui a 10 dias: #{futuro}" #=> Daqui a 10 dias: 2023-04-09

# Calcular uma data 20 dias no passado
passado = hoje - 20
puts "20 dias atrás: #{passado}" #=> 20 dias atrás: 2023-03-10

# Data um ano no futuro
prox_ano = hoje >> 12
puts "Ano que vem: #{prox_ano}" #=> Ano que vem: 2024-03-30
```

## Mergulho Profundo
Datas são essenciais para a programação desde o início dos computadores. No Ruby, a classe `Date` oferece métodos para manipular datas com facilidade.

Além de adicionar ou subtrair dias, é possível usar métodos como `next_day`, `prev_day`, `next_month`, `prev_month`, `next_year`, e `prev_year` para ajustes mais específicos. Existem alternativas como o método `advance` do ActiveSupport (Rails) que permite avançar ou retroceder datas em semanas, meses, anos de maneira mais explícita.

Há detalhes importantes ao lidar com datas, como fusos horários e anos bissextos, que o Ruby maneja internamente, mas que o desenvolvedor deve estar ciente para evitar surpresas ou bugs.

## Veja Também
- [Classe Date na documentação do Ruby](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [TimeAndDate.com para referência de fusos horários e anos bissextos](https://www.timeanddate.com/)
- [Stack Overflow: Ruby Date & Time Q&A](https://stackoverflow.com/questions/tagged/ruby+date)
