---
title:                "Analisando uma data a partir de uma string."
html_title:           "Elixir: Analisando uma data a partir de uma string."
simple_title:         "Analisando uma data a partir de uma string."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que e Por Que?
Parsing de datas a partir de strings é um processo de converter dados de formato de texto em um formato de data utilizável em programas. Programadores usam esse processo para a manipulação e armazenamento de datas em uma variedade de aplicações.

## Como Fazer:
Um exemplo simples de parsing de data usando a linguagem Elixir é utilizando a função `Date.from_iso8601/1`. Essa função aceita uma string no formato ISO 8601 (como "2021-01-01") e retorna um objeto `Date` correspondente. Veja um exemplo prático abaixo:

```Elixir
iex> Date.from_iso8601("2021-12-25")
{:ok, ~D[2021-12-25]}
```

## Profundando:
Nesse contexto, é importante entender a história por trás do ISO 8601, que é um padrão internacional para representar datas em um formato legível por máquinas. Existem outras formas de parsing de datas em Elixir, como a biblioteca `Timex`, que oferece mais opções de formatação e suporte a diferentes calendários. Internamente, o processo de parsing de datas envolve converter a string em um objeto de data e validar o formato seguindo as diretrizes definidas pelo ISO 8601.

## Veja Também:
- [Documentação oficial do Elixir sobre parse de datas](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
- [Página oficial do ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Documentação da biblioteca Timex](https://hexdocs.pm/timex/Timex.html#parse/2)