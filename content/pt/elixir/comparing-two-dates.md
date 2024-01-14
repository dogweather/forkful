---
title:    "Elixir: Comparando duas datas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas em Elixir?

Comparar duas datas é uma tarefa comum em qualquer linguagem de programação, e em Elixir não é diferente. Isso pode ser útil em situações como verificar uma data de nascimento para determinar a idade de uma pessoa, ou comparar datas para ordenar eventos em uma aplicação. Neste artigo, veremos como comparar duas datas em Elixir de forma eficiente.

## Como Fazer

Para comparar duas datas em Elixir, podemos usar a função `Date.compare/2`. Essa função recebe duas datas como argumentos e retorna um dos seguintes valores:

- `-1` se a primeira data for anterior à segunda data.
- `0` se as duas datas forem iguais.
- `1` se a primeira data for posterior à segunda data.

Por exemplo, vamos comparar duas datas utilizando a função `Date.compare/2`:

```Elixir
iex> Date.compare({2019, 10, 20}, {2019, 10, 22})
-1
iex> Date.compare({2020, 1, 1}, {2020, 1, 1})
0
iex> Date.compare({2021, 5, 10}, {2020, 12, 15})
1
```

Podemos também usar operadores comparativos diretamente nas datas. Elixir possui as funções `Date.<` e `Date.>` para comparar se uma data é menor ou maior que a outra, respectivamente. Por exemplo:

```Elixir
iex> {2020, 8, 30} < {2020, 9, 1}
true
iex> {2019, 10, 10} > {2015, 12, 1}
true
```

Outro recurso interessante é que podemos comparar o dia da semana de duas datas utilizando a função `Date.weekday/1`, que retorna um número de 1 a 7 representando o dia da semana, onde 1 é domingo e 7 é sábado. Por exemplo:

```Elixir
iex> Date.weekday({2021, 4, 30})
5
iex> Date.weekday({2021, 5, 1})
6
```

## Aprofundando-se

Além das funções mencionadas acima, Elixir possui outras maneiras de comparar duas datas de forma mais específica. Por exemplo, podemos usar a função `Date.diff/2` para obter a diferença entre duas datas em dias, meses, ou anos. Existem também funções para comparar apenas o ano, mês ou dia de duas datas.

Podemos ainda checar se duas datas são iguais utilizando as funções `Date.same?/2` e `Date.equal?/2`, que retornam `true` se as datas forem iguais e `false` caso contrário.

Com todas essas opções, podemos facilmente comparar duas datas de forma eficiente e precisa em Elixir.

## Veja Também

- [Documentação oficial do módulo `Date` em Elixir](https://hexdocs.pm/elixir/Date.html)
- [Tutorial sobre manipulação de datas em Elixir](https://theleakycauldronblog.com/date-and-time-in-elixir-7d0fb45a5201)
- [Guia para lidar com datas no Elixir](https://elixirschool.com/lessons/basics/datetime/)