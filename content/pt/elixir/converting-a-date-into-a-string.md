---
title:                "Elixir: Transformando uma data em uma sequência de caracteres"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa comum na programação, especialmente em Elixir. Isso pode ser útil para exibir a data de uma forma mais legível para o usuário final, ou para fazer comparações e cálculos mais precisos.

## Como fazer

Em Elixir, existem várias maneiras de converter uma data em uma string. A forma mais simples é usando a função `to_string/1`, que pega uma data e a converte em uma string no formato "YYYY-MM-DD". Veja um exemplo abaixo:

```Elixir
iex> date = ~D[2021-01-01]
iex> to_string(date)
"2021-01-01"
```

Para formatar a data de uma forma específica, podemos usar a biblioteca `Calendar` e suas funções `local_time/0` e `strftime!/2`. Você pode fornecer um formato personalizado usando os códigos de formatação, como `%Y` para o ano, `%m` para o mês e `%d` para o dia. Veja um exemplo abaixo:

```Elixir
iex> date = ~D[2021-01-01]
iex> Calendar.local_time(date) |> Calendar.strftime!("%d/%m/%Y")
"01/01/2021"
```

## Mergulho profundo

Ao converter uma data em uma string, é importante ter em mente a localidade e o fuso horário em que o código será executado. Elixir usa a biblioteca `Timex` para lidar com esses aspectos de data e hora, e é altamente recomendado usá-la ao trabalhar com datas em uma aplicação.

Outro fator importante é a validação da entrada. Certifique-se de que a data fornecida seja válida, caso contrário, o código pode gerar erros ou resultados imprecisos.

## Veja também

- [Documentação do módulo Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Documentação do módulo Timex](https://hexdocs.pm/timex/Timex.html)
- [Blog post sobre formatação de datas em Elixir](https://gorails.com/blog/working-with-dates-and-times-in-elixir)