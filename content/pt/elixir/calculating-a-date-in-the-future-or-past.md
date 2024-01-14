---
title:    "Elixir: Calculando uma data no futuro ou no passado"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que calcular datas no futuro e no passado em Elixir?

Se você está construindo um aplicativo de planejamento ou precisa automatizar tarefas relacionadas a datas, calcular datas no futuro e no passado pode ser uma tarefa importante. Com Elixir, podemos facilmente adicionar ou subtrair dias, meses ou anos a uma data específica. Neste post, vamos explorar como fazer isso de maneira eficiente e precisa.

## Como calcular datas no futuro e no passado em Elixir

Para calcular datas no futuro ou no passado em Elixir, o primeiro passo é converter a data em um formato específico. Para isso, usaremos o módulo `Calendar`. Vamos supor que queremos obter a data daqui a 30 dias a partir de hoje. O código ficaria assim:

```Elixir
today = Calendar.DateTime.now()
future_date = Calendar.DateTime.add(today, 30, :days)
```

No código acima, usamos a função `Calendar.DateTime.now()` para obter a data atual e armazená-la em uma variável. Em seguida, usamos a função `Calendar.DateTime.add()` para adicionar 30 dias a essa data atual. O terceiro argumento `:days` indica que estamos adicionando dias, mas você também pode adicionar meses, anos, horas, minutos, segundos, entre outros. A saída seria algo como `~N[2021-07-01 20:24:15]` dependendo da data e hora atuais.

Para calcular uma data no passado, podemos usar a função `Calendar.DateTime.subtract()`. Por exemplo, se quisermos obter a data de 30 dias atrás, o código ficaria assim:

```Elixir
today = Calendar.DateTime.now()
past_date = Calendar.DateTime.subtract(today, 30, :days)
```

E a saída seria algo como `~N[2021-05-31 20:24:15]`.

## Aprofundando no cálculo de datas em Elixir

O módulo `Calendar` oferece várias funções úteis para trabalhar com datas em Elixir. É importante notar que as datas são manipuladas como termos, e não como strings ou números. Isso significa que devemos usar o `~N` suffix para indicar que estamos trabalhando com um termo de data.

Além disso, podemos usar a função `Calendar.DateTime.to_date()` para converter uma data para um formato mais legível. Por exemplo:

```Elixir
date = Calendar.DateTime.now()
format_date = Calendar.DateTime.to_date(date)
```

A saída seria `~D[2021-06-30]`.

Você também pode adicionar ou subtrair datas com mais precisão, usando a função `days_difference()`. A seguinte função retorna o número de dias entre duas datas:

```Elixir
def days_difference(date1, date2) do
  diff = Calendar.DateTime.diff(date1, date2)
  Calendar.DateTime.to_gregorian_days(diff)
end
```

## Veja também

- [Documentação oficial do módulo `Calendar`](https://hexdocs.pm/elixir/Calendar.html)
- [Manipulando datas em Elixir com o módulo `Calendar`](https://dev.to/eiri/manipulating-dates-in-elixir-using-the-calendar-module-4l8b)