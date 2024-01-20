---
title:                "Trabalhando com arquivos csv"
html_title:           "Ruby: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

# O que é e por que usar?

Trabalhar com CSV (Comma-Separated Values, Valores Separados por Vírgula) é uma forma de gerenciar e manipular grandes quantidades de dados em formato de tabela. Programadores utilizam arquivos CSV para armazenar informações em formatos que podem ser facilmente compartilhados e acessados por outros sistemas.

# Como fazer:

Um arquivo CSV pode ser lido e manipulado usando a biblioteca padrão do Ruby. Veja um exemplo de leitura de um arquivo CSV e exibição dos seus dados:

```ruby
# Importando a biblioteca CSV
require "csv"

# Leitura do arquivo
csv_file = CSV.read("dados.csv")

# Exibindo os dados
csv_file.each do |linha|
  puts linha
end
```

A saída seria algo como:

```ruby
["Nome", "Idade", "E-mail"]
["Maria", "25", "maria@email.com"]
["João", "30", "joao@email.com"]
```

# Aprofundando:

A utilização de arquivos CSV é muito popular na área de programação, especialmente para troca de dados entre diferentes sistemas. Esses arquivos seguem um formato bastante simples, com cada linha sendo uma nova entrada de dados e cada coluna separada por uma vírgula.

Existem alternativas ao uso de arquivos CSV, tais como bancos de dados SQL ou XML. Porém, CSV continua sendo uma escolha comum devido à sua simplicidade e fácil manipulação.

No Ruby, é possível também criar, escrever e manipular arquivos CSV usando apenas a biblioteca padrão. Além disso, existem diversas bibliotecas de terceiros que adicionam funcionalidades extras para lidar com arquivos CSV de forma mais avançada.

# Veja também:

- [Documentação da biblioteca CSV do Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)