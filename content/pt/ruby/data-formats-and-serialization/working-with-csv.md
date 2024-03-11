---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:19.835549-07:00
description: "Trabalhar com arquivos CSV em Ruby oferece uma abordagem direta para\
  \ lidar com dados tabulares. Programadores frequentemente se engajam nesta pr\xE1\
  tica para\u2026"
lastmod: '2024-03-11T00:14:20.863731-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com arquivos CSV em Ruby oferece uma abordagem direta para lidar\
  \ com dados tabulares. Programadores frequentemente se engajam nesta pr\xE1tica\
  \ para\u2026"
title: Trabalhando com CSV
---

{{< edit_this_page >}}

## O Que & Por Quê?

Trabalhar com arquivos CSV em Ruby oferece uma abordagem direta para lidar com dados tabulares. Programadores frequentemente se engajam nesta prática para a análise de dados, extração, transformação e armazenamento, tornando-a uma habilidade crítica para tarefas que envolvem manipulação ou análise de dados.

## Como fazer:

Ruby inclui a biblioteca CSV por padrão, o que simplifica a leitura e escrita de arquivos CSV. Veja como você pode aproveitar isso para tarefas comuns:

### Lendo um arquivo CSV
Para ler de um arquivo CSV, você primeiro precisa da biblioteca CSV. Então, você pode iterar sobre as linhas ou lê-las em um array.

```ruby
require 'csv'

# Lendo cada linha como um array
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# A saída para cada linha pode parecer assim: ["data1", "data2", "data3"]
```

### Escrevendo em um CSV
Escrever em um arquivo CSV também é direto. Você pode adicionar a um arquivo existente ou criar um novo arquivo para escrever.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# Isso cria ou sobrescreve 'output.csv' com os cabeçalhos e valores especificados.
```

### Analisando uma string CSV
Às vezes, você precisa analisar dados CSV diretamente de uma string. Veja como:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# Saída esperada:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### Usando SmarterCSV
Para tarefas CSV mais complexas, o `SmarterCSV` pode ser uma ferramenta valiosa. Primeiro, instale a gema:

```shell
gem install smarter_csv
```

Então, você pode usá-la para lidar com arquivos grandes ou realizar análises e manipulações mais sofisticadas:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Isso lerá 'large_data.csv' e imprimirá cada linha como um hash baseado nos cabeçalhos.
```

Resumindo, a biblioteca CSV integrada do Ruby, juntamente com gemas de terceiros como `SmarterCSV`, fornece suporte robusto para o manuseio de dados CSV, permitindo tarefas eficientes de processamento e manipulação de dados.
