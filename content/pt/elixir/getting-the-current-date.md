---
title:                "Obtendo a data atual"
html_title:           "Elixir: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao escrever programas em Elixir, é necessário obter a data atual. Isso pode ser útil para gerar registros de data e hora ou para realizar cálculos de tempo. Felizmente, a linguagem possui uma função simples para obter a data atual.

## Como fazer

Para obter a data atual em Elixir, utilizamos a função `:calendar.universal_time/0`, que retorna a data e hora atual no formato de tupla:

```Elixir
{year, month, day, hour, minute, second}
```

Podemos então desestruturar essa tupla para obter os valores individuais:

```Elixir
{year, month, day, hour, minute, second} = :calendar.universal_time()
```

Para uma representação mais amigável, podemos utilizar a função `IO.inspect/2` para formatar a data e hora:

```Elixir
{year, month, day, hour, minute, second} = :calendar.universal_time()
IO.inspect("#{year}-#{month}-#{day} #{hour}:#{minute}:#{second}")
```

Isso resultará em algo como: `"2021-08-23 12:00:00"`, dependendo da data e hora atuais.

## Mergulho profundo

Vale mencionar que a função `:calendar.universal_time/0` retorna a data e hora no fuso horário Coordinated Universal Time (UTC). Se você precisar da data e hora em um fuso específico, pode utilizar a função `:calendar.universal_time_to_local_time/1` informando o número de horas de diferença em relação ao UTC.

Também é importante ter em mente que a função `:calendar.universal_time/0` é baseada no relógio do sistema operacional, portanto, qualquer alteração manual na data e hora do sistema refletirá no resultado dessa função.

## Veja também

- [Documentação oficial do módulo Calendar em Elixir](https://hexdocs.pm/elixir/Calendar.html)
- [Artigo sobre como obter a data atual em Elixir](https://elixirforum.com/t/get-current-date-time-in-elixir/12893)
- [Discussão sobre a função `:calendar.universal_time/0` no Elixir Talk](https://elixirforum.com/t/current-datetime/24937/9)