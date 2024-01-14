---
title:    "Elixir: Calculando uma data no futuro ou passado"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que realizar cálculos de datas com Elixir?

Calcular datas no futuro ou no passado pode ser uma necessidade comum em muitos aplicativos ou sistemas. Com a linguagem de programação Elixir, essa tarefa pode ser realizada de forma simples e eficiente. Neste artigo, discutiremos como podemos usar Elixir para calcular datas em nosso código.

## Como Fazer?

Para realizar cálculos de datas em Elixir, é necessário primeiro entender o formato de datas usado pela linguagem. Elixir usa o formato ISO 8601, que é um padrão internacional para representar datas e horários.

Para calcular uma data no futuro ou no passado, podemos usar a função `DateTime.add` e especificar o número de dias, semanas, meses ou anos que queremos adicionar ou subtrair. Por exemplo:

```elixir
DateTime.add(~U[2021-09-01 09:00:00], 7, :days)
```

Este código adicionará 7 dias à data especificada e retornará o resultado em formato ISO 8601.

Outra opção é usar a função `Date.add` se estivermos interessados apenas na data e não no horário. Esta função funciona da mesma forma que `DateTime.add` mas retorna apenas o formato de data. Por exemplo:

```elixir
Date.add(~D[2021-09-01], 2, :months)
```

Este código adicionará 2 meses à data especificada e retornará o resultado em formato ISO 8601.

Podemos também realizar cálculos com intervalos de tempo usando a função `Timex.Interval`. Por exemplo, se quisermos calcular uma data 1 ano e 6 meses depois da data atual, podemos fazer da seguinte forma:

```elixir
Timex.add(Timex.now, Timex.Interval.from_months(18))
```

Esta função adicionará 18 meses à data atual e retornará o resultado em formato ISO 8601.

## Mergulho Profundo

Além das funções mencionadas, Elixir também possui outras ferramentas úteis para cálculos de datas. Uma delas é o módulo `Calendar`, que nos permite obter informações específicas sobre uma data, como o dia da semana ou o número de dias em um mês específico.

Além disso, Elixir também possui bibliotecas externas, como o `Timex`, que fornece funcionalidades adicionais para manipulação de datas e horários. Ele oferece uma sintaxe mais amigável para cálculos de datas e possui uma documentação abrangente para facilitar o uso.

## Veja Também

- [Documentação do Elixir sobre cálculos de datas](https://hexdocs.pm/elixir/Calendar.html)
- [Documentação do Timex](https://hexdocs.pm/timex/readme.html)
- [ISO 8601](https://pt.wikipedia.org/wiki/ISO_8601)