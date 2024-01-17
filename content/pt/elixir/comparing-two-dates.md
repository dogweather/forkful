---
title:                "Comparando duas datas"
html_title:           "Elixir: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Comparar duas datas é uma tarefa comum na programação, que envolve verificar se duas datas são iguais, anteriores ou posteriores uma à outra. Os programadores geralmente fazem isso para garantir que as informações estejam organizadas corretamente e para tomar decisões baseadas em datas, como definir prazos ou agendar eventos.

## Como Fazer:
A linguagem de programação Elixir possui um módulo Date que oferece funções para comparar datas. Abaixo está um exemplo de como comparar duas datas usando esse módulo:

```Elixir 
date1 = ~D[2020-01-01] 
date2 = ~D[2020-01-15]
Date.compare(date1, date2)
```

O resultado será -1, indicando que a primeira data é anterior à segunda. Também podemos usar as funções `===` para verificar se as datas são iguais ou o operador `>` para verificar se uma data é posterior à outra.

## Mergulho Profundo:
Comparar datas é uma tarefa complexa que envolve lidar com diferenças entre calendários e anos bissextos. Ao usar o módulo Date em Elixir, essas diferenças são tratadas automaticamente, tornando a comparação mais precisa e confiável. Outras linguagens de programação, como Javascript, podem exigir bibliotecas externas para lidar com essas questões.

## Veja Também:
Para mais informações sobre o módulo Date em Elixir, confira a documentação oficial em: https://hexdocs.pm/elixir/Date.html