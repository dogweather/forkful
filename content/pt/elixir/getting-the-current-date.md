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

## O que e por que?

Obter a data atual é um recurso importante para programadores, pois permite que eles obtenham informações precisas sobre o tempo em que seu código é executado. Isso pode ser útil em muitos cenários, como armazenamento de informações de data e hora em bancos de dados, agendamento de tarefas e criação de registros de data e hora para depuração de erros.

## Como fazer:

Para obter a data atual em Elixir, podemos usar a função ```Date.utc_today``` que retorna uma data atualizada de acordo com o fuso horário UTC. Podemos formatar a data de várias maneiras, como mostrado abaixo:

```Elixir
Date.utc_today |> Date.to_iso8601
```

Esta linha de código irá retornar a data atual no formato ISO 8601, como "2021-05-17". Além disso, podemos usar a função ```DateTime.now``` para obter a data e hora atual com precisão até o milissegundo.

```Elixir
DateTime.now
```

Isso irá retornar um valor como "2021-05-17 14:30:00.123456Z". Também podemos utilizar outras funções, como ```Date.day_of_week``` e ```Date.day_of_year``` para obter informações adicionais sobre a data atual.

## Mergulho profundo:

Obter a data atual tem sido uma tarefa importante para programadores há muito tempo. Anteriormente, os programadores usavam funções como ```time``` e ```date``` para obter a data e hora do sistema. Mas com o tempo, novas linguagens e bibliotecas surgiram, tornando a obtenção da data atual uma tarefa mais simples e direta.

Além da função ```Date.utc_today```, também podemos usar a biblioteca ```calendar``` para obter informações sobre a data, como o número de dias em um determinado mês ou o dia da semana de uma data específica.

## Veja também:

- Documentação oficial Elixir: https://hexdocs.pm/elixir/Date.html
- Documentação da biblioteca calendar: https://hexdocs.pm/calendar/readme.html