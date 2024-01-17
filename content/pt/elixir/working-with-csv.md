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

## O que e por que?

A leitura e escrita de arquivos CSV é uma tarefa comum para programadores em muitos cenários, como a importação de dados em um banco de dados, a geração de relatórios ou a manipulação de dados em planilhas. CSV (Comma-Separated Values) é um formato de arquivo que armazena dados em uma tabela, onde cada linha contém registros separados por vírgulas. Isso o torna um formato popular para armazenar e trocar dados tabulares de forma simples e legível.

## Como fazer:

Para trabalhar com arquivos CSV em Elixir, podemos usar a biblioteca padrão ```File``` e sua função ```stream!```. Primeiro, precisamos abrir o arquivo CSV e transformá-lo em um stream de linhas:

```
file = File.stream!("dados.csv")
```

Em seguida, podemos usar a função ```CSV.decode/2``` para decodificar cada linha do stream em uma lista de dados, usando como separador a vírgula:

```
CSV.decode(file, headers: true, trim: true, separator: ",")
```

Isso criará uma lista de mapas contendo cada linha do arquivo CSV com seus respectivos campos, onde os nomes dos campos serão definidos pelos cabeçalhos do arquivo. Podemos então manipular esses dados conforme necessário.

## Deep Dive:

O formato CSV foi criado na década de 1970 como uma forma mais simples de troca de dados entre sistemas. Atualmente, existem muitas variações do padrão CSV, que podem causar problemas ao trabalhar com dados de diferentes fontes. Para lidar com essas variações, a biblioteca ```CSV``` do Elixir oferece opções de configuração, como o separador de campos e as aspas de caracteres.

Além disso, existem alternativas para trabalhar com dados CSV, como as bibliotecas "rcsv" e "floki". A biblioteca "rcsv" é baseada em leitura de arquivos e pode ser mais lenta no processamento de grandes conjuntos de dados. Já a biblioteca "floki" é baseada em parsing de XML e HTML e pode ser útil para fazer scrapers Web.

## Veja também:

- [Documentação oficial da biblioteca CSV do Elixir.](https://hexdocs.pm/csv/)
- [Tutorial sobre como trabalhar com CSV em Elixir.](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#working-with-csv-files)