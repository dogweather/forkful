---
title:                "Elixir: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou no passado?

Calcular datas pode ser uma tarefa essencial em muitos projetos de programação. Elixir tem algumas ferramentas úteis que podem ajudar a fazer esse cálculo de forma rápida e eficiente.

## Como fazer

A maneira mais simples de calcular uma data no futuro ou no passado em Elixir é usando as funções `Date.add/2` e `Date.subtract/2`. Essas funções aceitam dois argumentos: a data a ser alterada e o número de dias que devem ser adicionados ou subtraídos.

Vamos dar um exemplo simples de como usar essas funções para calcular uma data no futuro:

```Elixir
iex> hoje = Date.utc_today()
~D[2020-11-12]
iex> Date.add(hoje, 7)
~D[2020-11-19]
```

Podemos ver que a função `Date.add/2` adiciona 7 dias à data atual e retorna a nova data. Da mesma forma, podemos usar a função `Date.subtract/2` para calcular uma data no passado:

```Elixir
iex> hoje = Date.utc_today()
~D[2020-11-12]
iex> Date.subtract(hoje, 14)
~D[2020-10-29]
```

Uma coisa importante a notar é que essas funções retornam uma nova data e não alteram a data original. Para alterar a data original, podemos usar as funções `Date.add!/2` e `Date.subtract!/2`.

## Mergulho profundo

Além das funções `Date.add/2` e `Date.subtract/2`, Elixir também tem a função `Date.shift/2` que nos permite calcular datas futuras ou passadas com unidades de tempo mais precisas, como anos, meses, horas, minutos e segundos.

Além disso, o módulo `DateTime` é uma ótima ferramenta para trabalhar com datas e horários em Elixir. Ele fornece funções como `DateTime.add/4` e `DateTime.diff/3` que podem ser úteis ao realizar cálculos complexos com datas.

## Veja também

- [Documentação oficial do Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Documentação oficial do Elixir DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Post sobre cálculos de datas em Elixir no blog do AdopteUnDev](https://blog.adopteundev.com/calculation-de-dates-et-dheure-en-elixir-facilement/)