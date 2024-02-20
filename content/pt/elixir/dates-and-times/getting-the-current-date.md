---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:25.137812-07:00
description: "Obter a data atual em Elixir envolve acessar as informa\xE7\xF5es de\
  \ data e hora do sistema, uma tarefa comum para registro em log, marca\xE7\xE3o\
  \ de data ou qualquer\u2026"
lastmod: 2024-02-19 22:05:05.324792
model: gpt-4-0125-preview
summary: "Obter a data atual em Elixir envolve acessar as informa\xE7\xF5es de data\
  \ e hora do sistema, uma tarefa comum para registro em log, marca\xE7\xE3o de data\
  \ ou qualquer\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual em Elixir envolve acessar as informações de data e hora do sistema, uma tarefa comum para registro em log, marcação de data ou qualquer funcionalidade que requeira conhecimento da data atual. Esta operação é essencial para a criação de aplicações conscientes do tempo e para tarefas como gerar relatórios ou timestamps em uma aplicação web.

## Como fazer:
A biblioteca padrão do Elixir, por meio do módulo `DateTime`, permite buscar a data e hora atuais. Como o Elixir é executado na VM do Erlang (BEAM), ele aproveita as funcionalidades subjacentes do Erlang para operações com o tempo.

### Usando a Biblioteca Padrão do Elixir
O Elixir fornece a função `DateTime.utc_now/0` para obter a data e hora atuais em UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Saída de Exemplo:**
```
~U[2024-02-05 19:58:40.925931Z]
```

Para obter apenas a data atual, você pode extrair os componentes de ano, mês e dia:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Saída de Exemplo:**
```
~D[2023-05-04]
```

### Usando a Biblioteca Timex
Para requisitos de data-hora mais complexos, pode-se utilizar uma biblioteca de terceiros popular chamada Timex. Primeiramente, adicione `Timex` às suas dependências no mix.exs:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Após instalar a dependência (`mix deps.get`), você pode usar o Timex para obter a data atual:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Saída de Exemplo:**
```
~D[2023-05-04]
```

O Timex oferece funcionalidades extensivas para a manipulação de data-hora, tornando-se um poderoso acréscimo às suas aplicações em Elixir, especialmente quando lidando com fusos horários, formatação e análise de datas e horas.

Compreendendo e utilizando as capacidades integradas do Elixir e a biblioteca Timex, você pode trabalhar facilmente com datas e horas em suas aplicações Elixir, adequando a experiência às necessidades da sua aplicação com precisão e facilidade.
