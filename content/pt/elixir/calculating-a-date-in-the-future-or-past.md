---
title:                "Calculando uma data no futuro ou no passado."
html_title:           "Elixir: Calculando uma data no futuro ou no passado."
simple_title:         "Calculando uma data no futuro ou no passado."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em diversas situações, como por exemplo para agendar eventos, programar tarefas ou manipular datas em sistemas de reserva.

## Como Fazer

Para realizar cálculos de datas em Elixir, utilizamos a função `Date.add/2`, passando como argumentos a data original e o número de dias que queremos adicionar ou subtrair. Se quisermos calcular uma data no futuro, usamos um valor positivo para o número de dias, e se quisermos calcular uma data no passado, usamos um valor negativo. Vejamos um exemplo:

```elixir
iex> Date.utc_today()
{:ok, ~U[2022-01-25]}

iex> Date.add(~U[2022-01-25], 30)
~U[2022-02-24]
```

No exemplo acima, utilizamos a função `Date.utc_today/0` para obter a data de hoje e depois adicionamos 30 dias, resultando em uma data no futuro.

Podemos também fazer cálculos com datas em diferentes formatos, como no exemplo a seguir:

```elixir
iex> Date.add(~D[2022-03-10], -15)
~D[2022-02-23]
```

## Mergulho Profundo

Existem diversas outras funções disponíveis em Elixir para trabalhar com datas, como `Date.diff/2` para calcular a diferença entre duas datas, `Date.day_of_week/1` para obter o dia da semana de uma data específica e `Date.parse/1` para converter uma string em data. Além disso, é possível realizar operações mais complexas, como adicionar meses ou anos utilizando `Date.add/3`, especificando a unidade de tempo desejada.

## Veja Também

- [Documentação oficial de Elixir sobre manipulação de datas](https://hexdocs.pm/elixir/Date.html)
- [Tutorial sobre como trabalhar com datas em Elixir](https://ifelse.io/2016/02/10/working-with-dates-and-times-in-elixir/)