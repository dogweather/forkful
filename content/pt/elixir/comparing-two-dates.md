---
title:    "Elixir: Comparando duas datas"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que comparar duas datas em Elixir?

Comparar datas é uma tarefa comum em muitas linguagens de programação, e o Elixir não é exceção. Ao comparar duas datas, você pode determinar se uma data é anterior, posterior ou igual a outra. Isso pode ser útil em diversas situações, como criar lógicas de negócio baseado em datas ou ordenar eventos em ordem cronológica.

## Como comparar duas datas em Elixir

Para comparar duas datas em Elixir, podemos utilizar a função `Date.compare/2`. Essa função recebe como parâmetros duas datas e retorna o valor `:less` se a primeira data for anterior à segunda, `:equal` se as datas forem iguais e `:greater` se a primeira data for posterior à segunda.

```elixir
iex> Date.compare({2020, 12, 31}, {2021, 1, 1})
:less

iex> Date.compare({2021, 1, 1}, {2021, 1, 1})
:equal

iex> Date.compare({2021, 1, 1}, {2020, 12, 31})
:greater
```

Podemos, ainda, utilizar as funções `Date.before?/2` e `Date.after?/2` para recebermos como resultado um booleano `true` ou `false` dependendo da comparação realizada.

```elixir
iex> Date.before?({2020, 12, 31}, {2021, 1, 1})
true

iex> Date.before?({2021, 1, 1}, {2021, 1, 1})
false

iex> Date.after?({2021, 1, 1}, {2020, 12, 31})
true
```

## Mais sobre comparar datas em Elixir

Existem outras funções e módulos relacionados à comparação de datas em Elixir, como o `DateTime` e o `NaiveDateTime`. Essas estruturas são mais apropriadas para casos em que precisamos também lidar com informações de hora e fuso horário.

Podemos também usar a função `Date.diff/2` para calcular a diferença em dias entre duas datas.

Para mais informações e exemplos, acesse a documentação oficial da linguagem Elixir. 

## Veja também

- Documentação da linguagem Elixir: https://elixir-lang.org/docs.html
- Comparar datas com ExUnit: https://elixirforum.com/t/unit-tests-for-dates/23224
- Manipulação de datas com Elixir: https://www.youtube.com/watch?v=-UUZqN9VH5A