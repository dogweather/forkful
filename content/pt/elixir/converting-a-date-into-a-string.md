---
title:    "Elixir: Convertendo uma data em uma string"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa comum em muitos programas Elixir. Ao converter uma data para uma string, podemos visualizar a data de forma mais legível para os usuários e também realizar operações com mais facilidade.

## Como fazer:

Para converter uma data em uma string, podemos usar a função `~D` do módulo `Date` ou a função `~T` do módulo `DateTime`. Por exemplo:

```elixir
iex> Date.to_string(~D[2020-01-01])
"2020-01-01"

iex> DateTime.to_string(~T[2020-01-01 12:00:00])
"2020-01-01 12:00:00"
```

Além disso, também podemos formatar a saída da string usando a função `strftime`, que aceita diferentes opções de formatação. Por exemplo:

```elixir
iex> Date.strftime(~D[2020-01-01], "%B %d, %Y")
"January 01, 2020"
```

## Mergulho profundo:

Ao converter uma data em uma string, é importante ter em mente o formato de data padrão do sistema operacional em que o programa está sendo executado. Isso pode ser obtido usando a função `System.system_time/0`. Além disso, também é importante considerar a localização dos usuários, já que o formato de data pode variar de acordo com o idioma e a região.

## Veja também:

- Documentação do módulo `Date`: https://hexdocs.pm/elixir/Date.html
- Documentação do módulo `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Funções `strftime` e `system_time`: https://hexdocs.pm/elixir/1.11/Calendar.html#strftime/2