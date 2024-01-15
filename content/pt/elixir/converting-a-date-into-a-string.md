---
title:                "Convertendo uma data em uma string"
html_title:           "Elixir: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Converter datas em formato de string é uma tarefa comum em muitos projetos Elixir. Saber como realizar essa conversão é essencial para garantir a precisão e consistência dos dados em seu código.

## Como Fazer

A conversão de datas em strings no Elixir é feita através da função `~D`. Vamos ver um exemplo de como converter a data atual em uma string no formato "dd/mm/yyyy":

```elixir
~D[2020-05-22] |> NaiveDateTime.to_string() 
#=> "22/05/2020"
```

Você também pode especificar o formato da string, utilizando a função `~d` e um atalho para expressões do `strftime`:

```elixir
~d[2020-05-22] |> NaiveDateTime.to_string(~D |:d/ |:M |:yyyy)
#=> "22/05/2020"
```

A função `~D` também suporta a conversão de tempo em strings, simplesmente adicionando o formato de hora após o formato da data:

```elixir
~D[2020-05-22] |> NaiveDateTime.to_string(~D |:d/ |:M |:yyyy |:h |:m |:s)
#=> "22/05/2020 00:00:00"
```

## Mergulho Profundo

Ao converter datas em strings, é importante considerar o uso do módulo `Timex`, que oferece várias funções auxiliares para formatação de datas e horários. Também é importante ter em mente que as datas e horários no Elixir são imutáveis, o que significa que cada operação de conversão cria uma nova data em vez de modificar a existente.

## Veja Também

- [Documentação oficial do Elixir para a função `~D`](https://elixir-lang.org/getting-started/basic-types.html#todo)
- [Documentação do módulo `Timex`](https://hexdocs.pm/timex/)
- [Guia de formatação de datas com `Timex`](https://blog.appsignal.com/2019/07/09/how-to-format-dates-with-elixir-and-timex.html)