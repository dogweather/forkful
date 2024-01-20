---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Interpretando uma Data: Um Guia Elixir

## O quê e Por quê?

Interpretar uma data de uma string significa extrair (ou parse) uma data válida a partir de uma sequência de caracteres. Programadores fazem isso para converter dados de texto em tipos de dados mais úteis, como a data.

## Como fazer:

Utilizaremos a função parse/1 do módulo Date, que retorna a data numa tupla junto com o restante da string.

```elixir
iex> {:ok, data, resto} = Date.from_iso8601("2020-01-01extra")
{:ok, ~D[2020-01-01], "extra"}

iex> data
~D[2020-01-01]
```

Também podemos usar apenas um ‘!’ após o ‘from_iso8601’ para gerar um erro se a data não puder ser analisada.

```elixir
iex> Date.from_iso8601!("2020-01-32")
** (ArgumentError) argument error
```

## Deep Dive

1. **Contexto histórico**
   O Elixir, uma linguagem de programação funcional, concisa e versátil, foi criado em 2012. Ele integrou funcionalidades para manipulação de datas e strings a partir da versão 1.3.

2. **Alternativas**
   Outras abordagens para a interpretação de datas incluem a utilização de bibliotecas externas como a Timex. Também é possível criar uma função personalizada para tratar diferentes formatos de data.

3. **Detalhes de Implementação**
   A função from_iso8601/1 utiliza regular expressions para confirmar se a string segue o padrão YYYY-MM-DD. Caso siga, ela converte os segmentos da string em números, criando uma struct do tipo Date.

## Veja Também

Para mais informações, consulte a documentação oficial e blogs relacionados:

- Documentação oficial Elixir: [Date](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
- Postagem sobre manipulação de datas no Elixir: [Elixir School](https://elixirschool.com/en/lessons/basics/date-time/)

Não esqueça de explorar a biblioteca padrão do Elixir, pois ela possui muitas funcionalidades úteis. Continue aprendendo e experimentando!