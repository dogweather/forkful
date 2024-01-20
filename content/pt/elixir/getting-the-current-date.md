---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Programação em Elixir: Obtendo a Data Atual

## O Que & Por Quê?

Obter a data atual é um recurso comum em programação. É usado para registrar eventos, marcar postagens, lidar com expiração de sessões, entre outros. 

## Como fazer:

Em Elixir, você pode obter a data atual facilmente com o módulo `DateTime`. Veja os exemplos:

```Elixir
# Simplesmente pegar a data e a hora atuais:
DateTime.utc_now()
# Saída: ~U[2021-03-01T13:59:09.103Z]
```

```Elixir
# Apenas a data atual:
DateTime.to_date(DateTime.utc_now())
# Saída: ~D[2021-03-01]
```

## Mergulho Profundo:

A captura da data atual é um conceito que data de tempos em que os computadores eram programados para registrar eventos com uma marca de horário.

Existem outros módulos na linguagem Elixir para manipulação de datas e horários além da `DateTime`, como `Date` ou `Time`. Cada um com suas peculiaridades dependendo do uso.

O método `DateTime.utc_now()` retorna a data e a hora atuais no horário universal coordenado (UTC). Elixir usa estruturas em vez de objetos de data. Isso pode tornar a manipulação de datas mais simples, uma vez que não há orientação para objetos.

## Veja Também:

Links para documentações relacionadas para mais informações.

- DateTime em Elixir: https://hexdocs.pm/elixir/DateTime.html
- Módulo Date: https://hexdocs.pm/elixir/Date.html
- Time em Elixir: https://hexdocs.pm/elixir/Time.html