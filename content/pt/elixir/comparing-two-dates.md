---
title:                "Elixir: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Elixir?

Comparações de datas são comumente usadas em programas para verificar se uma data é mais antiga ou mais recente do que outra. Isso pode ser útil em diversos cenários, como em sistemas de reservas, agendamentos ou em aplicações financeiras.

## Como fazer a comparação de duas datas em Elixir

Em Elixir, podemos comparar datas utilizando o módulo `Calendar` e a função `date_to_erl`para converter as datas em formato `{:ok, date}` para formato `:erlang` que permite a comparação.

```Elixir
# Convertendo datas para formato de Erlang
{:ok, date1} = Calendar.Date.from_erl({2020, 10, 15})
{:ok, date2} = Calendar.Date.from_erl({2020, 12, 10})

# Comparando datas
date1 =< date2 # true
date1 > date2 # false
```

Podemos também utilizar a função `compare` para retornar um valor numérico que indica se a primeira data é anterior, posterior ou igual à segunda data.

```Elixir
Calendar.Date.compare(date1, date2) # -1
```

## Profundidade na comparação de datas em Elixir

Ao comparar duas datas, é importante entender como funciona o formato interno de datas em Elixir. Datas são armazenadas como a contagem de dias desde o dia 0 (01/01/0001). Então, ao comparar datas, estamos comparando esses valores numéricos internos.

Também é importante lembrar que o formato de Erlang usado em Elixir não suporta anos bissextos antes de 1900, portanto, datas nesse período serão tratadas de forma diferente.

## Veja também

- [Documentação do módulo Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Referência de funções do módulo Calendar](https://hexdocs.pm/elixir/Calendar.Functions.html)
- [Artigo sobre manipulação de datas em Elixir](https://medium.com/elixir-miracles/manipulação-de-datas-em-elixir-fac5991c0777)