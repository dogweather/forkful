---
title:                "Elixir: Calculando uma data no futuro ou no passado"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular uma data no futuro ou passado é uma tarefa comum ao escrever programas em Elixir. Saber como fazer isso pode te ajudar a criar aplicações mais dinâmicas e versáteis.

## Como Fazer

Existem várias maneiras de calcular uma data no futuro ou passado em Elixir, mas vamos focar em uma das mais simples e eficazes: usando a função `Timex.shift/4`. Essa função pertence à biblioteca `Timex`, que já vem incluída no Elixir.

Primeiro, precisamos importar a biblioteca `Timex` em nosso módulo:

```Elixir
import Timex
```

Agora, vamos ver alguns exemplos de como usar a função `Timex.shift/4` e a saída que ela produz:

```Elixir
# Para calcular 1 dia no futuro a partir da data atual:
Timex.shift(Timex.today(), days: 1)
# Saída: {{2020, 2, 11}, 0, 0, 0}

# Para calcular 1 semana no futuro a partir de uma data específica:
Timex.shift({2020, 2, 11}, weeks: 1)
# Saída: {{2020, 2, 18}, 0, 0, 0}

# Para calcular 1 mês no passado a partir da data atual:
Timex.shift(Timex.today(), months: -1)
# Saída: {{2020, 0, 10}, 0, 0, 0}

# Para calcular 1 ano no futuro a partir de uma data específica:
Timex.shift({2020, 2, 11}, years: 1)
# Saída: {{2021, 2, 11}, 0, 0, 0}

# Além de dias, semanas, meses e anos, é possível também calcular horas, minutos e segundos:
# Para calcular 6 horas no futuro a partir da data atual:
Timex.shift(Timex.today(), hours: 6)
# Saída: {{2020, 2, 11}, 6, 0, 0}
```

## Deep Dive

A função `Timex.shift/4` leva como primeiro argumento uma data e, em seguida, uma lista de opções para calcular a nova data. Essas opções podem ser dias, semanas, meses, anos, horas, minutos ou segundos, podendo ser tanto positivos (para calcular datas no futuro) quanto negativos (para calcular datas no passado).

Além disso, existe a opção `:calendar`, que aceita as opções `:gregorian` ou `:julian` e indica qual calendário deve ser usado para fazer o cálculo. Por padrão, o calendário gregoriano é usado.

Também é importante mencionar que a função `Timex.shift/4` retorna sempre uma `DateTime` na saída, independentemente do tipo de dado que foi passado como argumento.

Para mais informações sobre a função `Timex.shift/4` e outros recursos da biblioteca `Timex`, consulte a documentação oficial: https://hexdocs.pm/timex/Timex.html#shift/4

## Veja Também

- https://elixir-lang.org/getting-started/date-time-and-time-zones.html#calculating-dates-in-the-future
- https://hexdocs.pm/timex/Timex.html#shift/4
- https://hexdocs.pm/elixir/DateTime.html