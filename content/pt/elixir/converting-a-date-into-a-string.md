---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertendo uma data para uma string em Elixir

## O que é e por quê?

Converter uma data em uma string em Elixir significa transformar um objeto de data para uma representação legível de texto. Programadores fazem isso para formatar datas de maneiras específicas ou para imprimir datas de forma amigável.

## Como fazer:

Vamos fazer isso em algumas linhas de código usando a biblioteca padrão de Elixir.

```elixir
data = Date.new(2021, 12, 31)

IO.puts Date.to_string(data)
```

Resultado no console:

```elixir
"2021-12-31"
```

Pela biblioteca Timex:

```elixir
{:ok, data} = Date.new(2023, 5, 23)

Timex.format!(data, "{YYYY}-{0M}-{0D}", :strftime)
```

Resultado no console:

```elixir
"2023-05-23"
```

## Mergulho Profundo:

Em Elixir, a conversão de datas para strings era uma tarefa desafiadora em versões anteriores devido à falta de bibliotecas padrão. Com o lançamento da versão 1.3, Elixir introduziu a funcionalidade de trabalhar com datas através de módulos nativos.

Como alternativa à função padrão, também podemos usar a biblioteca Timex que fornece um conjunto mais completo e flexível de funcionalidades para o tratamento de datas.

Falando sobre os detalhes de implementação, a função `Date.to_string/1` converte uma data para um formato ISO 8601: "YYYY-MM-DD". Já com a biblioteca Timex, podemos definir nosso próprio formato de data.

## Veja também:

Para aprender mais sobre manipulação de datas em Elixir, veja os seguintes links:

1. Documentação oficial Elixir Date: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
2. Documentação oficial Timex: [https://hexdocs.pm/timex/Timex.html](https://hexdocs.pm/timex/Timex.html)
3. StackOverflow de perguntas frequentes sobre datas em Elixir: [https://stackoverflow.com/questions/tagged/elixir+date](https://stackoverflow.com/questions/tagged/elixir+date)