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

## Por que

Comparar duas datas pode ser uma tarefa comum e útil em vários cenários de programação. Algumas das razões mais comuns incluem verificar se um período de tempo já passou, calcular o tempo de duração entre duas datas ou determinar qual data é mais recente.

## Como fazer

Para comparar duas datas em Elixir, podemos usar o módulo `DateTime` juntamente com seus métodos `diff` e `compare`. Primeiro, precisamos definir as duas datas que queremos comparar:

```
iex> data1 = ~U[2021-01-01 23:59:59Z]
iex> data2 = ~U[2021-02-15 12:00:00Z]
```

Em seguida, podemos usar o método `compare` para verificar qual data é maior ou menor:

```
iex> DateTime.compare(data1, data2)
:lt
iex> DateTime.compare(data2, data1)
:gt
```

Neste exemplo, podemos ver que a data2 é maior (mais recente) do que a data1. Também podemos usar o método `diff` para obter o tempo de duração entre as duas datas:

```
iex> DateTime.diff(data1, data2)
~T[0, 7664141, :second] # resultado em formato: [dias, segundos, microssegundos]
```

## Mergulho profundo

Ao comparar duas datas em Elixir, é importante ter em mente que elas devem estar em formatos compatíveis. Portanto, se as datas não forem exibidas no mesmo fuso horário, pode ser necessário convertê-las usando o método `timezone` antes de fazer a comparação. Além disso, o método `compare` retorna o átomo `:eq` se as datas forem iguais, `lt` se a primeira data for menor que a segunda e `gt` se a primeira data for maior que a segunda.

## Veja também

- Documentação oficial sobre o módulo `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Artigo sobre a diferença entre métodos `diff` e `compare` em datas: https://www.amberbit.com/blog/comparing-dates-in-elixir/