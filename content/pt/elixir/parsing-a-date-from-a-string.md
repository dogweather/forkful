---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:35:45.228404-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

"Parsear" uma data a partir de uma string significa converter o texto para um formato padrão de data que o seu programa pode entender e manipular. Fazemos isso para que possamos, por exemplo, comparar datas, calcular intervalos de tempo ou simplesmente formatar a exibição corretamente.

## Como fazer:

```elixir
# Adicione a biblioteca Timex ao seu projeto mix.exs
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end

# Exemplo de parsing de uma data:
{:ok, datetime} = Timex.parse("2023-04-12", "{YYYY}-{0M}-{0D}")
IO.inspect(datetime)

# Saída esperada:
# ~N[2023-04-12 00:00:00]
```

## Mergulho Profundo

Parsear datas em Elixir já foi mais complicado, mas as liberdades da linguagem e a evolução das bibliotecas têm simplificado este processo. Antes do Timex, a biblioteca padrão, DateTime, lidava com algumas funções básicas de parsing, mas com limitações, especialmente em relação a fusos horários.

Timex é a escolha contemporânea para o Elixir, já que oferece uma API consistente e rica em funcionalidades para trabalhar com datas e horários. Além de parsing, você pode formatar, manipular e trabalhar com fusos horários de forma mais robusta.

Alternativamente, é possível utilizar a função `DateTime.from_iso8601/1` se você estiver trabalhando apenas com o formato ISO 8601 e não precisar de funcionalidades extras oferecidas pelo Timex.

O parsing de datas geralmente envolve alguns desafios: reconhecer formatos diferentes, lidar com fusos horários e normalizar para um formato padrão. Timex resolve isso usando "tokens" no formato de strings para entender como interpretar cada parte da data.

## Veja Também

- Documentação oficial do Timex: [https://hexdocs.pm/timex](https://hexdocs.pm/timex)
- Elixir `DateTime` módulo: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- Guia de introdução à linguagem Elixir: [https://elixir-lang.org/getting-started/introduction.html](https://elixir-lang.org/getting-started/introduction.html)