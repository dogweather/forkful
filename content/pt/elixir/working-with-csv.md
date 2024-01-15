---
title:                "Trabalhando com csv"
html_title:           "Elixir: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

Trabalhar com CSV (Comma-Separated Values) é útil para manipular grandes quantidades de dados tabulares. Com a ajuda do Elixir, você pode processar rapidamente um arquivo CSV e extrair as informações necessárias para suas tarefas.

## Como fazer?

Para trabalhar com CSV em Elixir, você precisará do módulo `CSV` da biblioteca padrão. Ele possui funções convenientes para ler, gravar e manipular arquivos CSV. Primeiro, vamos importar o módulo em nosso código:

```Elixir
import CSV 
```

Vamos supor que temos um arquivo CSV com dados de vendas de uma loja de roupas. Com o Elixir, podemos ler e armazenar esses dados em uma variável da seguinte maneira:

```Elixir
data = CSV.decode_file("vendas.csv")
```

Podemos, então, acessar os dados individualmente ou realizar operações neles, como filtrar por um determinado produto ou ano. Você também pode criar um novo arquivo CSV a partir dos dados processados.

## Mergulho profundo

O módulo `CSV` possui muitas funções úteis para trabalhar com arquivos CSV. Você pode definir o separador de campos, lidar com cabeçalhos de coluna, ignorar linhas em branco e muito mais. Além disso, se você precisar de mais controle, pode usar a biblioteca `Elixir CSV`, que oferece mais opções para lidar com arquivos CSV complexos.

## Veja também

- [Documentação oficial do módulo CSV](https://hexdocs.pm/elixir/CSV.html)
- [Biblioteca Elixir CSV](https://github.com/beatrichartz/csv)
- [Exemplo prático de uso de CSV em Elixir](https://medium.com/@jeffkreeftmeijer/postgres-and-elixir-transforming-data-using-csv-files-cc93f8ef539d)